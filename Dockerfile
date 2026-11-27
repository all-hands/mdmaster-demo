# syntax=docker/dockerfile:1

# INVARIANT: the moving tag that the digest below pins: debian:bookworm
FROM docker.io/library/debian:bookworm@sha256:268fb24c39bca92d326dd83ea3abebe4a3f7751c804ffef23bb6d857dd343ff8 AS build

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      build-essential ca-certificates curl git libffi-dev libgmp-dev \
      libncurses-dev libnuma-dev pkg-config xz-utils zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*

ARG GHCUP_VERSION=0.2.6.2
ARG GHCUP_SHA256=9ed5da5449b48043a0d17e767c05d2ef585e25a639bb934329496c6d2fad9cf8
ARG GHC_VERSION=9.10.3
ARG CABAL_VERSION=3.16.1.0

RUN curl --proto '=https' --tlsv1.2 -fsSL -o /usr/local/bin/ghcup \
      "https://downloads.haskell.org/~ghcup/${GHCUP_VERSION}/x86_64-linux-ghcup-${GHCUP_VERSION}" \
 && echo "${GHCUP_SHA256}  /usr/local/bin/ghcup" | sha256sum -c - \
 && chmod +x /usr/local/bin/ghcup

ENV GHCUP_INSTALL_BASE_PREFIX=/opt/ghcup
ENV PATH=/opt/ghcup/.ghcup/bin:$PATH
RUN ghcup install ghc "${GHC_VERSION}" --set \
 && ghcup install cabal "${CABAL_VERSION}" --set \
 && ghcup gc --cache --tmpdirs \
 && ghc --version && cabal --version

WORKDIR /build

COPY cabal.project cabal.project.freeze ./
COPY mdmaster-core/mdmaster-core.cabal       mdmaster-core/
COPY mdmaster-plugins/mdmaster-plugins.cabal mdmaster-plugins/
COPY mdmaster-app/mdmaster-app.cabal         mdmaster-app/
COPY mdmaster-web/mdmaster-web.cabal         mdmaster-web/

RUN cabal update && cabal build --only-dependencies all

COPY . .
RUN cabal build exe:mdmaster \
 && mkdir -p /out \
 && cp "$(cabal list-bin -v0 exe:mdmaster)" /out/mdmaster \
 && strip /out/mdmaster

RUN sh scripts/collect-licence-texts.sh /out/licenses

# INVARIANT: the moving tag that the digest below pins: debian:bookworm-slim
FROM docker.io/library/debian:bookworm-slim@sha256:734a72b3d6142a71993b4bc580ca01d8ca687f77a2c422d96a961d3376b179bc AS runtime

LABEL org.opencontainers.image.licenses="GPL-2.0-or-later"

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      libgmp10 libffi8 zlib1g ca-certificates \
 && rm -rf /var/lib/apt/lists/* \
 && useradd --system --create-home --shell /usr/sbin/nologin mdmaster

WORKDIR /app
COPY --from=build /out/mdmaster /usr/local/bin/mdmaster

COPY mdmaster-web/static /app/static
COPY LICENSE THIRD_PARTY_LICENSES.md /usr/share/doc/mdmaster/
COPY --from=build /out/licenses /usr/share/doc/mdmaster/licenses

USER mdmaster

ENV MDMASTER_PORT=8965 \
    MDMASTER_STATIC_DIR=/app/static

EXPOSE 8965

HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
  CMD bash -c 'exec 3<>/dev/tcp/127.0.0.1/8965 \
    && printf "GET /healthz HTTP/1.0\r\n\r\n" >&3 \
    && grep -qm1 "HTTP/1.[01] 200" <&3'

CMD ["mdmaster"]
