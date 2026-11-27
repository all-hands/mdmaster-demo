# Security policy

## Reporting a vulnerability

Please report a suspected vulnerability privately, through GitHub's private
vulnerability reporting: open the **Security** tab of this repository and
choose **Report a vulnerability**. Do not open a public issue for it.

We aim to acknowledge a report within 72 hours. Fixes go into the latest
version only; older versions are not patched.

## Scope

The demo's bundled sample files are made up. Visitors can also upload their own
files, so do not upload personal or confidential data to a demo instance you do not run yourself.

mdmaster is built to limit what an upload can cost the server:

- the app keeps uploads in memory and writes none to disk (a reverse proxy in
  front, such as nginx with request buffering on, may spool large bodies to
  its own temporary files);
- every file, batch, and expanded Word document has a size limit, and each
  request is checked against them before it is processed;
- results can be downloaded for a limited time and then expire;
- there are no accounts, and a result can be fetched only with the unguessable
  identifier that was returned when it was made.
