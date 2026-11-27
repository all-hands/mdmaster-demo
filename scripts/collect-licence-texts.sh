#!/bin/sh
set -eu

out=${1:?usage: collect-licence-texts.sh <output-dir>}
GHC=${GHC:-ghc}
ghcver=$("$GHC" --numeric-version)
store=$(cabal path --store-dir)/ghc-$ghcver
bootdoc=$(dirname "$(dirname "$("$GHC" --print-libdir)")")/../share/doc/x86_64-linux-ghc-$ghcver
table=${TABLE:-THIRD_PARTY_LICENSES.md}

packages=$(awk -F' *[|] *' '
  /^## Haskell packages linked/ { t = 1; next }
  /^## /                        { t = 0 }
  t && $2 != "" && $2 != "Package" && $2 !~ /^-+$/ { print $2 "-" $3 }
' "$table")
[ -n "$packages" ] || { echo "no packages found in $table" >&2; exit 1; }

exception() {
  case "$1" in
    erf-2.0.0.0)
      printf '%s\n' \
        "erf 2.0.0.0, by Lennart Augustsson. The package declares the licence" \
        "BSD-3-Clause but its release includes no licence file. The terms of" \
        "BSD-3-Clause follow." ""
      bsd3
      ;;
    *) return 1 ;;
  esac
}

bsd3() {
  cat <<'TEXT'
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
TEXT
}

mkdir -p "$out"
missing=""
for pv in $packages; do
  src=""
  case "$pv" in
    rts-*) src="$bootdoc/ghc-$ghcver" ;;
    *)
      for d in "$store"*/"$pv"-*/share/doc "$bootdoc/$pv"; do
        if [ -d "$d" ] && [ -n "$(ls -A "$d")" ]; then src=$d; break; fi
      done
      ;;
  esac
  mkdir -p "$out/$pv"
  if [ -n "$src" ]; then
    cp -R "$src"/. "$out/$pv/"
  elif ! exception "$pv" > "$out/$pv/LICENSE"; then
    rm -f "$out/$pv/LICENSE"; rmdir "$out/$pv"
    missing="$missing $pv"
  fi
done

if [ -n "$missing" ]; then
  echo "no licence text found for:$missing (add an exception or fix the lookup)" >&2
  exit 1
fi
echo "collected licence texts for $(echo "$packages" | wc -l) packages into $out"
