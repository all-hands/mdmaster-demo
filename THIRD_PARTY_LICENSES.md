# Third-party notices

mdmaster is licensed under the GNU General Public License, version 2 or (at your option) any later version, GPL-2.0-or-later (see [LICENSE](LICENSE)), the same licence as pandoc. Its executable and container image ship the third-party code listed below; the build tools and test-only packages the build plan also pins are listed apart, at the end.

## Licence compatibility

Some linked packages are under GPL licences: pandoc (GPL-2.0-or-later) and, through pandoc's dependencies, skylighting and texmath (GPL-2.0-only). mdmaster's GPL-2.0-or-later can be combined with all three under GPL version 2.

| Package | Version | Licence |
|---|---|---|
| pandoc | 3.14 | GPL-2.0-or-later |
| skylighting | 0.15.0.1 | GPL-2.0-only |
| texmath | 0.13.3.5 | GPL-2.0-only |

Two known upstream questions remain. They are in pandoc's own dependency closure, so pandoc's binary carries them too, and mdmaster records them here rather than settling them:

- skylighting is declared GPL-2.0-only, but one of the KDE syntax definitions it compiles in, `xml/systemverilog.xml` (module `Skylighting.Syntax.Systemverilog`), is marked GPL-3.0-or-later (`license="GPLv3+"`). GPL-3.0-or-later code cannot be distributed under GPL-2.0-only.
- unicode-data (linked through pandoc, commonmark and unicode-transforms) is Apache-2.0, which the FSF regards as incompatible with GPL version 2 (it is compatible with GPL version 3).

## Vendored JavaScript

**htmx** 2.0.11, licence 0BSD. File `mdmaster-web/static/htmx.min.js`.

- sha256: `d6fdc75f204e6bdefa99b69bf1e6d4ac69b8a364f77929f45c13476b4000f717`
- integrity (SRI): `sha384-2OatzQy1H+Zd/IIrjr1TcuDGqLXeHhbooAyJY1KdQMKnr4LZ22k31GBLdYKHmVjg`

## System libraries in the container image

The image's runtime stage installs these Debian bookworm packages; the executable links all of them dynamically except libffi8, which is installed but not linked. Their full copyright and licence texts are in the image at `/usr/share/doc/<package>/copyright`.

| Debian package | Library | Licence |
|---|---|---|
| libc6 | glibc | LGPL-2.1-or-later |
| libgmp10 | GMP | LGPL-3.0-or-later OR GPL-2.0-or-later |
| zlib1g | zlib | Zlib |
| libffi8 | libffi | MIT |

## Haskell packages linked into the executable

Every package the build plan pins in `cabal.project.freeze` that is in the dependency closure of the `mdmaster` executable (optional dependencies included, so a few are listed but not linked), with the licence its package description declares. The GHC runtime system and `base` are BSD-3-Clause. Each package's licence and notice files are in the container image at `/usr/share/doc/mdmaster/licenses/<package>-<version>/` (collected by `scripts/collect-licence-texts.sh`).

| Package | Version | Licence |
|---|---|---|
| aeson | 2.3.2.0 | BSD-3-Clause |
| aeson-pretty | 0.8.11 | BSD-3-Clause |
| ansi-terminal | 1.1.5 | BSD-3-Clause |
| ansi-terminal-types | 1.1.3 | BSD-3-Clause |
| appar | 0.1.8 | BSD-3-Clause |
| array | 0.5.8.0 | BSD-3-Clause |
| asciidoc | 0.1.1.1 | BSD-3-Clause |
| assoc | 1.1.1 | BSD-3-Clause |
| async | 2.2.6 | BSD-3-Clause |
| attoparsec | 0.14.4 | BSD-3-Clause |
| auto-update | 0.2.8 | MIT |
| base | 4.20.2.0 | BSD-3-Clause |
| base-compat | 0.15.0 | MIT |
| base-orphans | 0.9.4 | MIT |
| base16 | 2.0 | BSD-3-Clause |
| base16-bytestring | 1.0.2.0 | BSD-3-Clause |
| base64 | 2.1 | BSD-3-Clause |
| base64-bytestring | 1.2.1.0 | BSD-3-Clause |
| bifunctors | 5.6.3 | BSD-3-Clause |
| binary | 0.8.9.3 | BSD-3-Clause |
| bitvec | 1.1.6.0 | BSD-3-Clause |
| blaze-builder | 0.4.4.1 | BSD-3-Clause |
| blaze-html | 0.9.2.0 | BSD-3-Clause |
| blaze-markup | 0.8.3.0 | BSD-3-Clause |
| boring | 0.2.2.1 | BSD-3-Clause |
| bsb-http-chunked | 0.0.0.4 | BSD-3-Clause |
| byteorder | 1.0.4 | BSD-3-Clause |
| bytestring | 0.12.2.0 | BSD-3-Clause |
| call-stack | 0.4.0 | MIT |
| case-insensitive | 1.2.2.0 | BSD-3-Clause |
| cassava | 0.5.4.1 | BSD-3-Clause |
| character-ps | 0.1 | BSD-3-Clause |
| citeproc | 0.14.2 | BSD-2-Clause |
| colour | 2.3.7 | MIT |
| commonmark | 0.3.2 | BSD-3-Clause |
| commonmark-extensions | 0.2.8 | BSD-3-Clause |
| commonmark-pandoc | 0.3.0.2 | BSD-3-Clause |
| comonad | 5.0.10 | BSD-3-Clause |
| conduit | 1.3.6.1 | MIT |
| conduit-extra | 1.3.8 | MIT |
| constraints | 0.14.4 | BSD-2-Clause |
| containers | 0.7 | BSD-3-Clause |
| contravariant | 1.5.6 | BSD-3-Clause |
| cookie | 0.5.1 | MIT |
| cryptohash-md5 | 0.11.101.0 | BSD-3-Clause |
| crypton | 2.2.6 | BSD-3-Clause AND MIT AND ISC |
| crypton-asn1-encoding | 0.10.0 | BSD-3-Clause |
| crypton-asn1-parse | 0.10.0 | BSD-3-Clause |
| crypton-asn1-types | 0.4.1 | BSD-3-Clause |
| crypton-pem | 0.3.0 | BSD-3-Clause |
| crypton-x509 | 1.10.1 | BSD-3-Clause |
| data-default | 0.8.0.2 | BSD-3-Clause |
| data-default-class | 0.2.0.0 | BSD-3-Clause |
| data-fix | 0.3.4 | BSD-3-Clause |
| dec | 0.0.6 | BSD-3-Clause |
| deepseq | 1.5.0.0 | BSD-3-Clause |
| Diff | 2.0.1 | BSD-3-Clause |
| digest | 0.0.2.1 | BSD-2-Clause |
| directory | 1.3.8.5 | BSD-3-Clause |
| distributive | 0.6.3 | BSD-3-Clause |
| djot | 0.1.4.3 | MIT |
| dlist | 1.0 | BSD-3-Clause |
| doclayout | 0.6 | BSD-3-Clause |
| doctemplates | 0.11.1 | BSD-3-Clause |
| easy-file | 0.2.6 | BSD-3-Clause |
| emojis | 0.1.6 | BSD-3-Clause |
| erf | 2.0.0.0 | BSD-3-Clause |
| exceptions | 0.10.9 | BSD-3-Clause |
| fast-logger | 3.2.9 | BSD-3-Clause |
| file-embed | 0.0.16.0 | BSD-2-Clause |
| filepath | 1.5.4.0 | BSD-3-Clause |
| generics-sop | 0.5.1.4 | BSD-3-Clause |
| ghc-bignum | 1.3 | BSD-3-Clause |
| ghc-boot-th | 9.10.3 | BSD-3-Clause |
| ghc-internal | 9.1003.0 | BSD-3-Clause |
| ghc-prim | 0.12.0 | BSD-3-Clause |
| Glob | 0.10.2 | BSD-3-Clause |
| gridtables | 0.1.1.1 | MIT |
| haddock-library | 1.11.0 | BSD-2-Clause |
| hashable | 1.5.1.0 | BSD-3-Clause |
| http-api-data | 0.7 | BSD-3-Clause |
| http-date | 0.0.13 | BSD-3-Clause |
| http-media | 0.8.1.1 | MIT |
| http-semantics | 0.4.2 | BSD-3-Clause |
| http-types | 0.12.6 | BSD-3-Clause |
| http2 | 5.4.14 | BSD-3-Clause |
| HUnit | 1.6.2.0 | BSD-3-Clause |
| indexed-traversable | 0.1.5 | BSD-2-Clause |
| indexed-traversable-instances | 0.1.2.1 | BSD-2-Clause |
| integer-conversion | 0.1.1 | BSD-3-Clause |
| integer-gmp | 1.1 | BSD-3-Clause |
| integer-logarithms | 1.0.5 | MIT |
| iproute | 1.8.0 | BSD-3-Clause |
| ipynb | 0.2 | BSD-3-Clause |
| jira-wiki-markup | 1.5.1 | MIT |
| JuicyPixels | 3.3.10 | BSD-3-Clause |
| lucid | 2.11.20260427 | BSD-3-Clause |
| mime-types | 0.1.2.2 | MIT |
| mmorph | 1.2.2 | BSD-3-Clause |
| monad-control | 1.0.3.1 | BSD-3-Clause |
| mono-traversable | 1.0.21.0 | MIT |
| mtl | 2.3.1 | BSD-3-Clause |
| network | 3.2.12.0 | BSD-3-Clause |
| network-byte-order | 0.1.9 | BSD-3-Clause |
| network-control | 0.1.7 | BSD-3-Clause |
| network-uri | 2.6.4.2 | BSD-3-Clause |
| old-locale | 1.0.0.7 | BSD-3-Clause |
| OneTuple | 0.4.3 | BSD-3-Clause |
| Only | 0.1 | BSD-3-Clause |
| optparse-applicative | 0.19.0.0 | BSD-3-Clause |
| ordered-containers | 0.2.5 | BSD-3-Clause |
| os-string | 2.0.7 | BSD-3-Clause |
| pandoc | 3.14 | GPL-2.0-or-later |
| pandoc-types | 1.23.1.2 | BSD-3-Clause |
| parsec | 3.1.18.0 | BSD-2-Clause |
| pretty | 1.1.3.6 | BSD-3-Clause |
| prettyprinter | 1.7.2 | BSD-2-Clause |
| prettyprinter-ansi-terminal | 1.1.4 | BSD-2-Clause |
| primitive | 0.9.1.0 | BSD-3-Clause |
| process | 1.6.26.1 | BSD-3-Clause |
| psqueues | 0.2.8.3 | BSD-3-Clause |
| QuickCheck | 2.18.0.0 | BSD-3-Clause |
| quote-quot | 0.2.2.0 | BSD-3-Clause |
| ram | 0.22.1 | BSD-3-Clause |
| random | 1.3.1 | BSD-3-Clause |
| recv | 0.1.3 | BSD-3-Clause |
| regex-base | 0.94.0.3 | BSD-3-Clause |
| regex-tdfa | 1.3.2.7 | BSD-3-Clause |
| resourcet | 1.3.0 | BSD-3-Clause |
| rts | 1.0.2 | BSD-3-Clause |
| safe | 0.3.21 | BSD-3-Clause |
| scientific | 0.3.10.0 | BSD-3-Clause |
| semialign | 1.4 | BSD-3-Clause |
| semigroupoids | 6.0.2 | BSD-2-Clause |
| servant | 0.20.3.0 | BSD-3-Clause |
| servant-lucid | 0.9.0.6 | BSD-3-Clause |
| servant-server | 0.20.3.0 | BSD-3-Clause |
| simple-sendfile | 0.2.33 | BSD-3-Clause |
| singleton-bool | 0.1.8 | BSD-3-Clause |
| skylighting | 0.15.0.1 | GPL-2.0-only |
| skylighting-core | 0.15.0.1 | BSD-3-Clause |
| skylighting-format-ansi | 0.1 | BSD-3-Clause |
| skylighting-format-blaze-html | 0.1.2.2 | BSD-3-Clause |
| skylighting-format-context | 0.1.0.2 | BSD-3-Clause |
| skylighting-format-latex | 0.1 | BSD-3-Clause |
| skylighting-format-typst | 0.1 | BSD-3-Clause |
| some | 1.1 | BSD-3-Clause |
| sop-core | 0.5.0.2 | BSD-3-Clause |
| split | 0.2.5.1 | BSD-3-Clause |
| splitmix | 0.1.3.2 | BSD-3-Clause |
| StateVar | 1.2.2 | BSD-3-Clause |
| stm | 2.5.3.1 | BSD-3-Clause |
| streaming-commons | 0.2.3.1 | MIT |
| strict | 0.5.1 | BSD-3-Clause |
| syb | 0.7.4 | BSD-3-Clause |
| tagged | 0.8.11 | BSD-3-Clause |
| tagsoup | 0.14.8 | BSD-3-Clause |
| template-haskell | 2.22.0.0 | BSD-3-Clause |
| template-haskell-lift | 0.1.0.0 | BSD-2-Clause |
| temporary | 1.3 | BSD-3-Clause |
| texmath | 0.13.3.5 | GPL-2.0-only |
| text | 2.1.3 | BSD-2-Clause |
| text-builder | 1.0.0.5 | MIT |
| text-builder-core | 0.1.1.3 | MIT |
| text-builder-linear | 0.1.4 | BSD-3-Clause |
| text-conversions | 0.3.1.1 | ISC |
| text-iso8601 | 0.1.1.2 | BSD-3-Clause |
| text-short | 0.1.6.1 | BSD-3-Clause |
| th-abstraction | 0.7.2.0 | ISC |
| th-compat | 0.1.7 | BSD-3-Clause |
| th-lift | 0.8.7 | BSD-3-Clause |
| th-lift-instances | 0.1.20 | BSD-3-Clause |
| these | 1.2.1 | BSD-3-Clause |
| time | 1.12.2 | BSD-2-Clause |
| time-compat | 1.9.9 | BSD-3-Clause |
| time-hourglass | 0.3.0 | BSD-3-Clause |
| time-manager | 0.5.2 | MIT |
| toml-parser | 2.0.2.0 | ISC |
| transformers | 0.6.1.1 | BSD-3-Clause |
| transformers-base | 0.4.6.1 | BSD-3-Clause |
| transformers-compat | 0.7.2 | BSD-3-Clause |
| typed-process | 0.2.13.0 | MIT |
| typst | 0.12.1 | BSD-3-Clause |
| typst-symbols | 0.3 | MIT |
| unicode-collation | 0.1.3.7 | BSD-2-Clause |
| unicode-data | 0.9.0 | Apache-2.0 |
| unicode-transforms | 0.4.0.1 | BSD-3-Clause |
| uniplate | 1.6.13 | BSD-3-Clause |
| unix | 2.8.7.0 | BSD-3-Clause |
| unix-compat | 0.7.4.1 | BSD-3-Clause |
| unix-time | 0.6.0 | BSD-3-Clause |
| unliftio-core | 0.2.1.0 | MIT |
| unordered-containers | 0.2.21 | BSD-3-Clause |
| utf8-string | 1.0.2 | BSD-3-Clause |
| uuid-types | 1.0.6.1 | BSD-3-Clause |
| vault | 0.3.2.0 | BSD-3-Clause |
| vector | 0.13.2.0 | BSD-3-Clause |
| vector-algorithms | 0.9.1.0 | BSD-3-Clause |
| vector-stream | 0.1.0.1 | BSD-3-Clause |
| wai | 3.2.5 | MIT |
| wai-app-static | 3.2.1 | MIT |
| wai-extra | 3.1.18 | MIT |
| wai-logger | 2.5.1 | BSD-3-Clause |
| warp | 3.4.21 | MIT |
| witherable | 0.5 | BSD-3-Clause |
| word8 | 0.1.4 | BSD-3-Clause |
| xml | 1.3.14 | BSD-3-Clause |
| xml-conduit | 1.10.1.0 | MIT |
| xml-types | 0.3.8 | MIT |
| yamlet | 1.0.0.1 | BSD-3-Clause |
| yamlet-aeson | 1.0.0.1 | BSD-3-Clause |
| zip-archive | 0.5 | BSD-3-Clause |
| zlib | 0.7.1.1 | BSD-3-Clause |

## Build tools and test-only packages (not shipped)

Pinned by `cabal.project.freeze` and used to build or test mdmaster, but not linked into the executable and not in the image.

| Package | Version | Licence |
|---|---|---|
| alex | 3.5.4.2 | BSD3 |
| attoparsec-aeson | 2.2.2.0 | BSD-3-Clause |
| Cabal | 3.12.1.0 | BSD-3-Clause |
| cabal-doctest | 1.0.13 | BSD-3-Clause |
| Cabal-syntax | 3.12.1.0 | BSD-3-Clause |
| cmdargs | 0.10.22 | BSD3 |
| happy | 2.2 | BSD2 |
| happy-lib | 2.2 | BSD-2-Clause |
| haskell-lexer | 1.2.1 | MIT |
| hsc2hs | 0.68.10 | BSD3 |
| hspec | 2.11.18 | MIT |
| hspec-core | 2.11.18 | MIT |
| hspec-discover | 2.11.18 | MIT |
| hspec-expectations | 0.8.4 | MIT |
| hspec-wai | 0.12.1 | MIT |
| pretty-show | 1.10 | MIT |
| quickcheck-io | 0.2.0 | MIT |
| tasty | 1.5.5 | MIT |
| terminal-size | 0.3.4 | BSD3 |
