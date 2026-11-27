# mdmaster demo

Operations on Markdown files, in the browser: insert or update a table of
contents, convert Word documents to Markdown with their comment threads and
images, rewrite a document in one canonical form, and strip HTML comments.
Markdown results can be read as a diff, and every successful result can be
downloaded.

The [user guide](USER_GUIDE.md) walks through a whole session, from choosing an
operation to downloading the zip, and explains each option.

This repository is the demo. It starts with made-up sample files from
Thistlewick Island, a place that does not exist. Pick an operation to load its
samples, press **Run**, and click a file to see what changed. Your own files
work too; the app keeps them in memory only (a reverse proxy in front may
buffer large uploads to its own temporary files).

## About this repository

This repository is a generated, one-way export of the application's source.
`main` holds a single commit, and each new export replaces it, so the history
is not kept here. Changes made here are overwritten by the next export.

Pull requests here are not merged. Bug reports are welcome as issues. To report
a vulnerability, do not open a public issue: see [SECURITY.md](SECURITY.md).

## Run it

```bash
podman build --format docker -t mdmaster-demo .
podman run --rm -p 8965:8965 mdmaster-demo
```

(`docker build -t mdmaster-demo .` works as well.) The image builds on
x86_64 (amd64) only. Then open
<http://localhost:8965>. A cold build compiles pandoc and takes a quarter of an
hour or more.

## Run it as a service

`deploy/mdmaster.container` is a podman quadlet unit that root installs and
runs. It starts `localhost/mdmaster-demo:latest` from root's image store
(`/var/lib/containers/storage`), which does not hold images built by your own
user, so build the image as root:

```bash
sudo podman build --format docker -t mdmaster-demo .
```

Install the unit as root, then start it:

```bash
sudo cp deploy/mdmaster.container /etc/containers/systemd/mdmaster.container
sudo systemctl daemon-reload
sudo systemctl start mdmaster.service
```

The unit starts at boot through its `[Install]` section: quadlet applies that
when it generates the unit, so `systemctl enable` does not work on generated
units (`podman-systemd.unit(5)`). Logs go to the journal: `journalctl -u mdmaster`.

`deploy/nginx-mdmaster.conf` is an example reverse proxy in front of it. Put its
rate-limit zones in `/etc/nginx/nginx.conf` or a snippet under
`/etc/nginx/conf.d/` (nginx refuses them inside a server block). Copy the file to
`/etc/nginx/sites-available/` and link it from `sites-enabled/`, set
`server_name` to the host name visitors use, and add a `listen 443 ssl` block
once a certificate exists (certbot's nginx plugin does this). Once HTTPS works,
un-comment the HSTS `add_header` line. Restore the visitor's address if another
reverse proxy sits in front, then run `sudo nginx -t && sudo systemctl reload nginx`.

## Samples

| Folder | Operation |
|---|---|
| `mdmaster-web/static/demo/toc` | Table of Contents |
| `mdmaster-web/static/demo/docx` | DOCX to Markdown |
| `mdmaster-web/static/demo/normalize` | Normalize |
| `mdmaster-web/static/demo/strip-comments` | Strip Comments |

The Word documents are built by `python3 scripts/make_demo_docx.py`
(standard library only); `--check` verifies the committed files.

`MDMASTER_DEMO=0` turns demo mode off; the file
`mdmaster-web/static/demo/DEMO_MODE` turns it on.

## Test

GHC 9.10.3 and cabal 3.16.1.0: `cabal test all --with-compiler=ghc-9.10.3`.
The flag is needed when another `ghc` comes first on `PATH`: without it, cabal
fails in dependency resolution.

CI here is manual (`workflow_dispatch` only): a push or a pull request starts
no run. Most source comments are left out of this export; the ones tagged
`KEEP:`, `INVARIANT:`, `WARNING:` or `SECURITY:` are the comments it keeps.

## License

GPL-2.0-or-later (the GNU General Public License, version 2 or any later
version), the same licence as pandoc; see [LICENSE](LICENSE). The third-party
code the application ships, with its licences, is listed in
[THIRD_PARTY_LICENSES.md](THIRD_PARTY_LICENSES.md).

If you distribute mdmaster, changed or not (including as a container image),
the licence requires you to offer the corresponding source with it.
