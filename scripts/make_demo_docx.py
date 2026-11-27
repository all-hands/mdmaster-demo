#!/usr/bin/env python3
import pathlib
import struct
import sys
import zipfile
import zlib
from xml.sax.saxutils import escape

OUT = pathlib.Path(__file__).resolve().parent.parent / "mdmaster-web/static/demo/docx"

W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
W14 = "http://schemas.microsoft.com/office/word/2010/wordml"
W15 = "http://schemas.microsoft.com/office/word/2012/wordml"
MC = "http://schemas.openxmlformats.org/markup-compatibility/2006"
PKGREL = "http://schemas.openxmlformats.org/package/2006/relationships"
OREL = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
MSREL = "http://schemas.microsoft.com/office/2011/relationships"
WP = "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
A = "http://schemas.openxmlformats.org/drawingml/2006/main"
PIC = "http://schemas.openxmlformats.org/drawingml/2006/picture"
CT = "application/vnd.openxmlformats-officedocument.wordprocessingml"
XML = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
NS = (f'xmlns:w="{W}" xmlns:w14="{W14}" xmlns:mc="{MC}" mc:Ignorable="w14" '
      f'xmlns:r="{OREL}" xmlns:wp="{WP}" xmlns:a="{A}" xmlns:pic="{PIC}"')


def png(width, height, palette, pixel):
    raw = b"".join(b"\0" + bytes(pixel(x, y) for x in range(width)) for y in range(height))
    blocks = [raw[i:i + 65535] for i in range(0, len(raw), 65535)]
    body = b"".join(
        struct.pack("<BHH", i == len(blocks) - 1, len(b), len(b) ^ 0xFFFF) + b
        for i, b in enumerate(blocks))
    idat = b"\x78\x01" + body + struct.pack(">I", zlib.adler32(raw))

    def chunk(kind, data):
        return (struct.pack(">I", len(data)) + kind + data
                + struct.pack(">I", zlib.crc32(kind + data)))

    return (b"\x89PNG\r\n\x1a\n"
            + chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 3, 0, 0, 0))
            + chunk(b"PLTE", b"".join(bytes(c) for c in palette))
            + chunk(b"IDAT", idat) + chunk(b"IEND", b""))


def lighthouse():
    def at(x, y):
        if y > 82:
            return 4
        if 70 <= x < 90 and 34 <= y <= 82:
            return 2 if (y // 8) % 2 else 1
        if 68 <= x < 92 and 24 <= y < 34:
            return 3
        if x >= 92 and abs(y - 29) * 4 < x - 92 + 8:
            return 5
        return 0
    return png(160, 96, [(38, 34, 74), (240, 240, 245), (200, 60, 80),
                         (250, 210, 90), (40, 90, 140), (120, 110, 90)], at)


def moth():
    def at(x, y):
        dx, dy = abs(x - 60), y - 40
        if dx < 4 and abs(dy) < 26:
            return 2
        if (dx - 26) ** 2 + (dy + 8) ** 2 < 420 or (dx - 20) ** 2 + (dy - 14) ** 2 < 220:
            return 3 if (dx - 28) ** 2 + (dy + 8) ** 2 < 40 else 1
        return 0
    return png(120, 80, [(245, 242, 253), (164, 139, 255), (60, 50, 90), (255, 111, 174)], at)


class Doc:
    def __init__(self):
        self.body, self.comments, self.extended = [], [], []
        self.footnotes, self.images, self.links = [], [], []

    def t(self, text, props=""):
        rpr = f"<w:rPr>{props}</w:rPr>" if props else ""
        return f'<w:r>{rpr}<w:t xml:space="preserve">{escape(text)}</w:t></w:r>'

    def b(self, text):
        return self.t(text, "<w:b/>")

    def i(self, text):
        return self.t(text, "<w:i/>")

    def link(self, text, url):
        self.links.append(url)
        return f'<w:hyperlink r:id="rIdL{len(self.links)}">{self.t(text)}</w:hyperlink>'

    def note(self, text):
        n = len(self.footnotes) + 2
        self.footnotes.append((n, text))
        return f'<w:r><w:footnoteReference w:id="{n}"/></w:r>'

    def anchored(self, text, *thread, done=False):
        ids, root = [], None
        for author, date, words in thread:
            cid = len(self.comments)
            para = f"{0x5A000001 + cid:08X}"
            ids.append(cid)
            self.comments.append(
                f'<w:comment w:id="{cid}" w:author="{escape(author)}" '
                f'w:initials="{"".join(p[0] for p in author.split())}" w:date="{date}">'
                f'<w:p w14:paraId="{para}">{self.t(words)}</w:p></w:comment>')
            parent = f' w15:paraIdParent="{root}"' if root else ""
            self.extended.append(
                f'<w15:commentEx w15:paraId="{para}"{parent} w15:done="{int(done)}"/>')
            root = root or para
        return ("".join(f'<w:commentRangeStart w:id="{c}"/>' for c in ids) + self.t(text)
                + "".join(f'<w:commentRangeEnd w:id="{c}"/>' for c in ids)
                + "".join(f'<w:r><w:commentReference w:id="{c}"/></w:r>' for c in ids))

    def para(self, *runs, props=""):
        ppr = f"<w:pPr>{props}</w:pPr>" if props else ""
        self.body.append(f"<w:p>{ppr}{''.join(runs)}</w:p>")

    def h(self, level, text):
        self.para(self.t(text), props=f'<w:pStyle w:val="Heading{level}"/>')

    def items(self, rows, numbered=False):
        for row in rows:
            runs = row if isinstance(row, tuple) else (self.t(row),)
            self.para(*runs, props='<w:pStyle w:val="ListParagraph"/><w:numPr><w:ilvl w:val="0"/>'
                                   f'<w:numId w:val="{2 if numbered else 1}"/></w:numPr>')

    def table(self, rows):
        def cell(content, bold):
            runs = content if content.startswith("<w:") else (self.b if bold else self.t)(content)
            return f'<w:tc><w:tcPr><w:tcW w:w="2400" w:type="dxa"/></w:tcPr><w:p>{runs}</w:p></w:tc>'
        grid = "".join('<w:gridCol w:w="2400"/>' for _ in rows[0])
        trs = "".join(
            "<w:tr>" + ("<w:trPr><w:tblHeader/></w:trPr>" if n == 0 else "")
            + "".join(cell(c, n == 0) for c in row) + "</w:tr>"
            for n, row in enumerate(rows))
        self.body.append('<w:tbl><w:tblPr><w:tblW w:w="0" w:type="auto"/>'
                         '<w:tblLook w:val="0020" w:firstRow="1"/></w:tblPr>'
                         f"<w:tblGrid>{grid}</w:tblGrid>{trs}</w:tbl>")

    def image(self, data, alt, width, height):
        self.images.append(data)
        n = len(self.images)
        cx, cy = width * 19050, height * 19050
        self.para(
            f'<w:r><w:drawing><wp:inline distT="0" distB="0" distL="0" distR="0">'
            f'<wp:extent cx="{cx}" cy="{cy}"/><wp:docPr id="{n}" name="Picture {n}" '
            f'descr="{escape(alt)}"/><a:graphic><a:graphicData uri="{PIC}"><pic:pic><pic:nvPicPr>'
            f'<pic:cNvPr id="{n}" name="image{n}.png" descr="{escape(alt)}"/><pic:cNvPicPr/>'
            f'</pic:nvPicPr><pic:blipFill><a:blip r:embed="rIdI{n}"/><a:stretch><a:fillRect/>'
            f'</a:stretch></pic:blipFill><pic:spPr><a:xfrm><a:off x="0" y="0"/>'
            f'<a:ext cx="{cx}" cy="{cy}"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom>'
            f'</pic:spPr></pic:pic></a:graphicData></a:graphic></wp:inline></w:drawing></w:r>')

    def entries(self):
        types, rels, parts = [], [], []

        def part(name, kind, rel, xml):
            types.append(f'<Override PartName="/word/{name}" ContentType="{CT}.{kind}+xml"/>')
            rels.append(f'<Relationship Id="rIdP{len(rels)}" Type="{rel}" Target="{name}"/>')
            parts.append((f"word/{name}", XML + xml))

        part("styles.xml", "styles", f"{OREL}/styles", STYLES)
        part("numbering.xml", "numbering", f"{OREL}/numbering", NUMBERING)
        if self.footnotes:
            notes = "".join(
                f'<w:footnote w:id="{n}"><w:p>{self.t(text)}</w:p></w:footnote>'
                for n, text in self.footnotes)
            part("footnotes.xml", "footnotes", f"{OREL}/footnotes",
                 f"<w:footnotes {NS}>{notes}</w:footnotes>")
        if self.comments:
            part("comments.xml", "comments", f"{OREL}/comments",
                 f"<w:comments {NS}>{''.join(self.comments)}</w:comments>")
            part("commentsExtended.xml", "commentsExtended", f"{MSREL}/commentsExtended",
                 f'<w15:commentsEx xmlns:w15="{W15}" xmlns:mc="{MC}" mc:Ignorable="w15">'
                 f"{''.join(self.extended)}</w15:commentsEx>")
        rels += [f'<Relationship Id="rIdI{n}" Type="{OREL}/image" Target="media/image{n}.png"/>'
                 for n in range(1, len(self.images) + 1)]
        rels += [f'<Relationship Id="rIdL{n}" Type="{OREL}/hyperlink" Target="{escape(u)}" '
                 'TargetMode="External"/>' for n, u in enumerate(self.links, 1)]
        media = [(f"word/media/image{n}.png", d) for n, d in enumerate(self.images, 1)]
        return [
            ("[Content_Types].xml", XML
             + '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">'
               '<Default Extension="rels" '
               'ContentType="application/vnd.openxmlformats-package.relationships+xml"/>'
               '<Default Extension="xml" ContentType="application/xml"/>'
               '<Default Extension="png" ContentType="image/png"/>'
               f'<Override PartName="/word/document.xml" ContentType="{CT}.document.main+xml"/>'
               + "".join(types) + "</Types>"),
            ("_rels/.rels", XML + f'<Relationships xmlns="{PKGREL}"><Relationship Id="rId1" '
                                  f'Type="{OREL}/officeDocument" Target="word/document.xml"/>'
                                  "</Relationships>"),
            ("word/document.xml", XML + f"<w:document {NS}><w:body>{''.join(self.body)}"
                                        "</w:body></w:document>"),
            ("word/_rels/document.xml.rels", XML + f'<Relationships xmlns="{PKGREL}">'
                                                   f"{''.join(rels)}</Relationships>"),
        ] + parts + media


STYLES = (f'<w:styles xmlns:w="{W}">'
          '<w:style w:type="paragraph" w:default="1" w:styleId="Normal"><w:name w:val="Normal"/>'
          "</w:style>"
          + "".join(
              f'<w:style w:type="paragraph" w:styleId="Heading{n}"><w:name w:val="heading {n}"/>'
              f'<w:basedOn w:val="Normal"/><w:pPr><w:outlineLvl w:val="{n - 1}"/></w:pPr>'
              f'<w:rPr><w:b/><w:sz w:val="{36 - 4 * n}"/></w:rPr></w:style>' for n in (1, 2, 3))
          + '<w:style w:type="paragraph" w:styleId="ListParagraph">'
            '<w:name w:val="List Paragraph"/><w:basedOn w:val="Normal"/></w:style></w:styles>')

NUMBERING = (f'<w:numbering xmlns:w="{W}">'
             + "".join(
                 f'<w:abstractNum w:abstractNumId="{n}"><w:lvl w:ilvl="0"><w:start w:val="1"/>'
                 f'<w:numFmt w:val="{fmt}"/><w:lvlText w:val="{text}"/></w:lvl></w:abstractNum>'
                 for n, fmt, text in ((1, "bullet", "&#8226;"), (2, "decimal", "%1.")))
             + "".join(f'<w:num w:numId="{n}"><w:abstractNumId w:val="{n}"/></w:num>'
                       for n in (1, 2)) + "</w:numbering>")


ODALYS, PIM, WREN = "Odalys Brambleshaw", "Pim Ottersley", "Wren Tallowmere"


def moth_post():
    d = Doc()
    d.h(1, "Proposal: A Moth Post for Thistlewick Island")
    d.para(d.t("The gulls have resigned. This paper proposes that letters be carried by "),
           d.b("trained moths"), d.t(", who are quieter and "),
           d.anchored("almost never eat the stamps",
                      (ODALYS, "2031-03-02T09:00:00Z", "Almost never? Do we have a number?"),
                      (PIM, "2031-03-02T10:20:00Z", "Two stamps in forty deliveries. Both were cheese-flavoured."),
                      (ODALYS, "2031-03-02T10:31:00Z", "Then let us stop printing those.")),
           d.t("."))
    d.image(moth(), "A courier moth", 120, 80)
    d.h(2, "How it works")
    d.items(["The sender ties the letter with blue thread.",
             "The moth is shown a map and a warm lamp.",
             (d.t("The letter arrives "), d.i("before"), d.t(" breakfast."))], numbered=True)
    d.h(2, "Costs")
    d.table([["Item", "Buttons per year", "Remarks"],
             ["Moths (30)", "45", "includes hats"],
             ["Lamps", d.anchored("12", (WREN, "2031-03-03T08:15:00Z",
                                         "The lighthouse offered to lend us theirs.")), "oil extra"],
             ["Apologies to the gulls", "3", "one time"]])
    d.h(2, "Risks")
    d.items([(d.anchored("Woollen jumpers",
                         (WREN, "2031-03-03T08:20:00Z", "We agreed to post the moths in cotton bags."),
                         done=True), d.t(" in the sorting office")),
             "Full moons", "Jealous gulls"])
    d.para(d.t("A decision is needed before the fog season."),
           d.note("The fog season starts when the fog says so."))
    return d


def council_minutes():
    d = Doc()
    d.h(1, "Minutes of the Tide Council")
    d.para(d.b("Present: "), d.t("O. Brambleshaw (chair), P. Ottersley, W. Tallowmere, the goat"))
    d.h(2, "1. The two hundred and thirteenth step")
    d.para(d.t("The council walked the stairs. "),
           d.anchored("The count was 212 up and 213 down.",
                      (WREN, "2031-04-11T19:05:00Z", "I counted 214. Please minute my objection."),
                      (PIM, "2031-04-11T19:06:00Z", "One of those was the goat.")),
           d.t(" The matter was adjourned."),
           d.note("For the ninth year in a row."))
    d.h(2, "2. Singing umbrella")
    d.para(d.t("It was resolved to lend the umbrella to the "),
           d.link("Museum of Lost Umbrellas", "https://example.org/umbrellas"), d.t("."))
    d.h(2, "3. Actions")
    d.table([["Who", "What", "By"],
             ["Pim", "Oil both bells", "Moonday"],
             ["Wren", "Count again, without goat", "Starday"],
             ["Goat", "None accepted", "never"]])
    return d


def leaflet():
    d = Doc()
    d.h(1, "Welcome to Thistlewick Island")
    d.image(lighthouse(), "The lighthouse at night", 160, 96)
    d.para(d.t("Population 91, "), d.i("not counting clouds"), d.t("."))
    d.h(2, "Things to see")
    d.items([(d.b("The lighthouse"), d.t(" (bring a limerick)")),
             (d.b("The cloud observatory"), d.t(" (closed on clear days)")),
             (d.b("The bakery"), d.t(" (try the invisible tart)"))])
    d.h(2, "Getting here")
    d.para(d.t("Take the ferry from Port Quillander. "),
           d.link("Timetable", "https://example.org/ferry"), d.t("."))
    return d


DOCS = {"moth-post-proposal.docx": moth_post, "tide-council-minutes.docx": council_minutes,
        "visitor-leaflet.docx": leaflet}


def build(doc, path):
    with zipfile.ZipFile(path, "w", zipfile.ZIP_STORED) as z:
        for name, data in doc.entries():
            info = zipfile.ZipInfo(name, date_time=(1980, 1, 1, 0, 0, 0))
            info.create_system = 3
            info.external_attr = 0o600 << 16
            z.writestr(info, data)


def main():
    import io
    failed = False
    for name, make in DOCS.items():
        buffer = io.BytesIO()
        build(make(), buffer)
        target = OUT / name
        if "--check" in sys.argv[1:]:
            same = target.is_file() and target.read_bytes() == buffer.getvalue()
            print(("ok      " if same else "differs ") + name)
            failed = failed or not same
        else:
            OUT.mkdir(parents=True, exist_ok=True)
            target.write_bytes(buffer.getvalue())
            print("wrote   " + name, len(buffer.getvalue()))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
