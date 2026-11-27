# mdmaster user guide

mdmaster runs one of four operations on a batch of Markdown or Word files, shows
you what each file would become, and lets you download the results. This guide
walks through a whole session in the demo, from opening the page to downloading
a zip. The same steps apply to your own files.

The demo uses made-up sample files about a place that does not exist. Nothing in
them refers to a real organisation or person. Your own files work too, and the
rules for them are in [Your own files](#your-own-files).

## Before you start

Open the page. The top of the page shows a short notice that this is the demo,
and below it a panel of sample files. Each operation has its own set of samples.

You do not need an account, a password or any setting. The filter box and the
page needs JavaScript: choosing files, loading samples, running a batch, the
filter box and the keyboard shortcuts all depend on it.

## 1. Choose an operation

The first control is **Operation**, a drop-down list with four choices:

| Operation | What it does | Input |
|---|---|---|
| Table of Contents | Inserts, refreshes or removes a table of contents in a Markdown document | `.md`, `.markdown` |
| DOCX to Markdown | Converts a Word document to Markdown, with its comments and images | `.docx` |
| Normalize | Rewrites a Markdown document in one canonical form | `.md`, `.markdown` |
| Strip Comments | Removes every HTML comment from a Markdown document | `.md`, `.markdown` |

The operation you pick decides what the page accepts. Choosing DOCX to Markdown
makes the page accept `.docx` files. Choosing one of the Markdown operations
makes it accept `.md` and `.markdown` files. Files of any other type are not
uploaded.

Each operation shows its options underneath the drop-down list. Changing the
operation replaces those options. If the drop zone holds only samples, changing
the operation also replaces the samples with the new operation's set.

## 2. Add your files

You can add files in three ways:

- drag files or a whole folder onto the drop zone;
- press **Choose files** and pick one or more files;
- press **Choose a folder** to add everything in a folder.

Once files are in the zone, they appear in a list with a count. **Clear all**
removes them. The line under the zone states how many files may be uploaded, the
largest size of each file, and, for Word documents, the largest size a file may
expand to when it is opened. These figures are the limits of the running
instance, and they are shown on the page so that an oversized file can be seen
before you upload.

The defaults are up to 500 files in one batch, 10 MiB per file and 100 MiB per
batch. For Word documents the defaults are 50 MiB per document once expanded and
200 MiB for all the Word documents in a batch. A `.docx` file is a zip archive,
so its size on disk can be much smaller than the text it holds. The server
checks both: the uploaded size against the per-file and per-batch limits, and
then the expanded size against the Word limits. A file can be refused on either.

Samples in the demo go into the zone the same way a file you pick does, so you
can run them straight away.

## 3. Set the options

Each operation has its own options. The panel explains each one beside its
control. A value outside the allowed range is refused with a message next to the
field, and the run does not start until it is corrected.

### Table of Contents

- **Mode**: *Update* (the default) inserts a table of contents or refreshes an
  existing one. *Remove* deletes the table of contents and the anchors it added,
  and leaves the content of the rest of the document unchanged. The whole
  document is written out again, so spacing and Markdown spelling (for
  example list markers or emphasis characters) may change.
- **Depth**: the deepest heading level included, from 1 to 6. The default is 3,
  which means headings at `#`, `##` and `###`.
- **Skip title**: *Auto* (the default), *Always* or *Never*. Under *Auto* the
  first heading is left out of the table only when it is a top-level heading, it
  is the first heading, and it is the only top-level heading.
- **Anchors**: *GitHub* (the default) and *Pandoc* link to headings the way those
  renderers expect. *Explicit* adds an anchor tag before each included heading,
  so the links work in any Markdown renderer. The anchors carry a
  `data-mdmaster` attribute, which is how mdmaster finds and replaces them on the
  next run.
- **Style**: *Bullets* (the default) writes a nested bullet list. *Numbered*
  writes a nested numbered list.

The table of contents sits inside a marker block:

```markdown
<!-- mdmaster:toc start -->

- [Setup](#setup)
- [Running](#running)

<!-- mdmaster:toc end -->
```

If the document already has this block, mdmaster rewrites its contents in place,
wherever the block sits. If it does not, the block goes after the first
top-level heading when the document opens with one. When anything comes before
that heading (a paragraph, for example), or there is no top-level heading, the
block goes at the top. A start marker with no end
marker is an error for that file, because mdmaster does not guess where the
block should end. Each marker belongs on a line of its own; the end marker may
follow the last line of the old table directly. A marker that shares its block
with other HTML (inside a `<div>`, for example) is an error too, because
replacing the block would delete that HTML. Running the operation twice with the same options gives the
same result.

### Normalize

- **Smart punctuation**: *Off* (the default) leaves the document's own quotes
  and dashes alone. *On* turns straight quotes into curly ones, turns `--` into
  an en dash, `---` into an em dash, and `...` into an ellipsis.

Normalize parses the document and writes it out again. Constructs that have more
than one spelling land on one spelling. For example, setext headings, asterisk
bullets and underscore emphasis are all written the way mdmaster's own writer
writes them. Line breaks are kept as you wrote them, so a document wrapped at 80
columns is not joined into long lines. Code blocks, inline code, raw HTML and
link targets are not changed. YAML front matter is kept exactly as written,
comment lines and key order included. If a file is already in canonical form, it is
reported as unchanged. That is a quick way to see whether a document would change
before you process a whole folder.

### Strip Comments

- **Keep mdmaster markers**: *True* (the default) keeps the table of contents
  marker comments, so a later Table of Contents run still finds its block. *False*
  removes them as well.

Strip Comments removes HTML comments written on their own lines and comments
inside a paragraph. Other raw HTML is not changed. If a comment sat inside a
sentence, the spaces around it are tidied so the sentence reads as if the
comment had never been there.

### DOCX to Markdown

- **Comments**: how Word comments are written into the Markdown.
  - *Review* (the default) puts a quote after each annotated block, with replies
    nested one level. It reads well in most Markdown viewers.
  - *Diff-stable* gathers every thread under one `## Comments` heading at the
    end. The prose is left untouched, so two conversions of the same document
    differ only where the document itself differs.
  - *Agent* writes each thread as a list of keyed values, with a key taken from
    Word's comment identifier, so a key survives later edits. It is meant for
    machines to read.
  - *Archive* keeps every field of every comment as an HTML comment that
    displays as nothing, followed by the comment text. It keeps the most: the
    author, initials, date, resolved state and the reply links that Word
    recorded. Inside these fields every `\`, `"` and `-` is written with a `\`
    before it, so a value can never close the comment; drop each `\` to read a
    value back (`2031\-03\-02` is `2031-03-02`).

Whichever style you choose, the prose is the same, and threads and replies are
kept. The styles differ in the details: *Diff-stable* writes the anchor, author,
day and comment text but not whether a comment was resolved; *Review* and
*Agent* mark resolved comments; *Archive* keeps every field mdmaster recovers.
Details Word stores in optional parts of the file that cannot be read are left
out rather than guessed. If you need a complete record, keep the original
`.docx` as well. Comments anchored inside a table, a
figure or a footnote are carried across too. A comment anchored anywhere inside
a table is attached to the table as a whole, so in the styles that place
comments next to what they annotate, it appears after the table.

The output is named for what it is. `notes.docx` comes back as `notes.md`, both
in the download and inside the zip.

Embedded images are carried across. Each image becomes a file in a `media` folder
beside the converted document, and the Markdown refers to it from there. If two
documents in the same folder embed different images under the same name, both
images are kept, and the second one gets a numbered name. An image that cannot
be found inside the document is left out, and the output says so where the image
would have been. Images in headers and footers are not extracted.

## 4. Run

Press **Run**. The button shows a spinner while the batch is processed. Each run
has a processing time limit, 60 seconds by default. The timer starts once the
upload has been received and checked, so the time it takes to send the files
does not count against it. A run that goes past the limit is
stopped and refused. The server also serves only a few runs at once. When it is
busy, it refuses the run and asks you to wait 30 seconds before trying again.

## 5. Read the results

The results appear under the form. The headline states what happened: that
nothing was processed, that every file failed, or how many files were processed.
Below it is a list with one row for each file. A row shows the file's path and
its status.

Select a row to open its diff in the pane beside the list. The diff shows the
lines that changed, with the words that changed marked inside them. Above the
diff, a control sets how much unchanged text is shown around each change: *less*,
*standard* (the default, 3 lines), *more* (10 lines) or *50 lines*. Unchanged
text farther than 50 lines from any change is not shown.

Three situations need a note:

- If a file already looks exactly as the operation would leave it, the pane says
  so instead of showing a diff.
- A file that failed shows its error message in the pane. Open the row to read
  why it failed.
- A DOCX to Markdown result has no diff, because there is no Markdown before the
  conversion to compare against. The pane is left out for those rows. Each
  converted file can still be downloaded, and images appear as rows of their own
  with their own download.

When a batch has more than ten files, a **Filter files** box appears above the
list. Type part of a path to narrow the list. A count shows how many files are
visible. The filter only hides rows. It does not change the results or the
download.

With the list showing, the arrow keys (or `j` and `k`) move through the rows that
have a diff, and `Home` and `End` jump to the first and last such row. Each step
opens that row's diff, as a click does.

## 6. Download

There are three ways to download:

- **Download** on a row saves that one file. The file takes the name the operation
  gives it, so `notes.docx` becomes `notes.md`.
- **Copy** copies a converted Markdown file's text to the clipboard. The button is
  in the header of that row's diff pane, which opens when you click the row. It
  appears on Markdown rows whose diff is shown, changed or unchanged. It does not appear for failed files, for the images of a DOCX conversion, or for
  a DOCX conversion's row, which opens no pane.
- **Download all as zip** saves every result in one archive. Image files sit in
  the zip beside the document that refers to them. The zip also holds a
  `MANIFEST.md` file (if one of your results already has that name, the
  manifest is called `mdmaster-MANIFEST.md`, or gets a number). It records the operation, the option values the run used,
  the UTC time of the run, and a table with one row per file. Each row gives the
  file's path, its status, and how many lines were added and removed, where
  counting applies. The manifest does not include the batch identifier, because
  that identifier is the key to the results.

The zip is not offered when every file in the batch failed, since it would be
empty. Open a failed file to see why.

## 7. Results expire

Results are kept for a limited time, at most 30 minutes by default. The results
head says how many minutes remain at most. When the server is short of room it
removes the results closest to expiry first, so a result can go before its time
is up: save what you need promptly. When the time is up, the download links stop working,
and the page shows that the result has expired. Download what you need before
then, or upload the files again. **Start over** on the expired page returns to
the form.

Results are held in the server's memory only. When they expire, or when the
server restarts, they are gone. The server does not keep a copy of your files,
accounts or history.

## Your own files

You can process your own files in the demo. Choose the operation, add the files
as described above, and press Run. The files are sent to the server, kept in
memory by the app (a proxy in front may buffer large uploads to temporary
files), and the results are discarded when they
expire. Please do not send personal or confidential information to a public demo.
Use a copy of mdmaster that you run yourself for that.

Some points to keep in mind when you use your own files:

- A Markdown file that is not valid UTF-8 is refused with a message saying so.
  Save it as UTF-8 first.
- A Table of Contents start marker with no matching end marker is reported as an
  error for that file. Add the missing end marker and run again. The same goes
  for a marker inside a larger HTML block: give it a line of its own.
- Running a Markdown operation (Table of Contents, Normalize, Strip comments) on
  its own output again gives the same result, so a second run with the same
  options changes nothing. DOCX to Markdown takes `.docx` files only, so it
  cannot be run on its own output.

## Running mdmaster yourself

The demo repository's README explains how to build and run the container on your
own machine, and it opens on port 8965. The limits and the expiry time can be
changed by whoever runs the instance. The page shows the values in force, so
check them there rather than relying on the defaults in this guide.

## Licence and source

mdmaster is free software under the GNU General Public License, version 2 or
later. The footer of the page links to the source code of the instance you are
using.
