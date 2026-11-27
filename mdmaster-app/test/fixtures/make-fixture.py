#!/usr/bin/env python3
import pathlib
import struct
import sys
import zipfile
import zlib

W      = 'http://schemas.openxmlformats.org/wordprocessingml/2006/main'
W14    = 'http://schemas.microsoft.com/office/word/2010/wordml'
W15    = 'http://schemas.microsoft.com/office/word/2012/wordml'
W16CID = 'http://schemas.microsoft.com/office/word/2016/wordml/cid'
W16CEX = 'http://schemas.microsoft.com/office/word/2018/wordml/cex'
MC     = 'http://schemas.openxmlformats.org/markup-compatibility/2006'
PKGREL = 'http://schemas.openxmlformats.org/package/2006/relationships'
OREL   = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships'
MSREL  = 'http://schemas.microsoft.com/office/2011/relationships'
MSREL18 = 'http://schemas.microsoft.com/office/2018/relationships'
MSREL16 = 'http://schemas.microsoft.com/office/2016/relationships'
WP   = 'http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing'
A    = 'http://schemas.openxmlformats.org/drawingml/2006/main'
PIC  = 'http://schemas.openxmlformats.org/drawingml/2006/picture'
CT = 'application/vnd.openxmlformats-officedocument.wordprocessingml'

CONTENT_TYPES = f'''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
  <Default Extension="xml" ContentType="application/xml"/>
  <Default Extension="png" ContentType="image/png"/>
  <Override PartName="/word/document.xml" ContentType="{CT}.document.main+xml"/>
  <Override PartName="/word/comments.xml" ContentType="{CT}.comments+xml"/>
  <Override PartName="/word/commentsExtended.xml" ContentType="{CT}.commentsExtended+xml"/>
  <Override PartName="/word/commentsIds.xml" ContentType="{CT}.commentsIds+xml"/>
  <Override PartName="/word/commentsExtensible.xml" ContentType="{CT}.commentsExtensible+xml"/>
</Types>'''

RELS = f'''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="{PKGREL}">
  <Relationship Id="rId1" Type="{OREL}/officeDocument" Target="word/document.xml"/>
</Relationships>'''

DOC_RELS = f'''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="{PKGREL}">
  <Relationship Id="rId1" Type="{OREL}/comments" Target="comments.xml"/>
  <Relationship Id="rId2" Type="{MSREL}/commentsExtended" Target="commentsExtended.xml"/>
  <Relationship Id="rId3" Type="{MSREL18}/commentsExtensible" Target="commentsExtensible.xml"/>
  <Relationship Id="rId4" Type="{MSREL16}/commentsIds" Target="commentsIds.xml"/>
  <Relationship Id="rId5" Type="{OREL}/image" Target="media/image1.png"/>
</Relationships>'''

DOCUMENT = f'''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:document xmlns:w="{W}" xmlns:w14="{W14}" xmlns:mc="{MC}" mc:Ignorable="w14"
            xmlns:r="{OREL}" xmlns:wp="{WP}" xmlns:a="{A}" xmlns:pic="{PIC}">
  <w:body>
    <w:p w14:paraId="10000001">
      <w:r><w:t xml:space="preserve">Intro sentence before any comment. </w:t></w:r>
      <w:commentRangeStart w:id="0"/>
      <w:commentRangeStart w:id="1"/>
      <w:commentRangeStart w:id="2"/>
      <w:r><w:t>shared range</w:t></w:r>
      <w:commentRangeEnd w:id="0"/>
      <w:commentRangeEnd w:id="1"/>
      <w:commentRangeEnd w:id="2"/>
      <w:r><w:commentReference w:id="0"/></w:r>
      <w:r><w:commentReference w:id="1"/></w:r>
      <w:r><w:commentReference w:id="2"/></w:r>
      <w:r><w:t xml:space="preserve"> tail of the first paragraph.</w:t></w:r>
    </w:p>
    <w:p w14:paraId="10000002">
      <w:commentRangeStart w:id="3"/>
      <w:r><w:t>resolved anchor</w:t></w:r>
      <w:commentRangeEnd w:id="3"/>
      <w:r><w:commentReference w:id="3"/></w:r>
      <w:r><w:t xml:space="preserve"> and more body text. </w:t></w:r>
      <w:commentRangeStart w:id="4"/>
      <w:r><w:t>long note anchor</w:t></w:r>
      <w:commentRangeEnd w:id="4"/>
      <w:r><w:commentReference w:id="4"/></w:r>
    </w:p>
    <w:p w14:paraId="10000003">
      <w:r>
        <w:drawing>
          <wp:inline distT="0" distB="0" distL="0" distR="0">
            <wp:extent cx="190500" cy="190500"/>
            <wp:docPr id="1" name="Picture 1"/>
            <a:graphic>
              <a:graphicData uri="{PIC}">
                <pic:pic>
                  <pic:nvPicPr>
                    <pic:cNvPr id="1" name="image1.png"/>
                    <pic:cNvPicPr/>
                  </pic:nvPicPr>
                  <pic:blipFill>
                    <a:blip r:embed="rId5"/>
                    <a:stretch><a:fillRect/></a:stretch>
                  </pic:blipFill>
                  <pic:spPr>
                    <a:xfrm><a:off x="0" y="0"/><a:ext cx="190500" cy="190500"/></a:xfrm>
                    <a:prstGeom prst="rect"><a:avLst/></a:prstGeom>
                  </pic:spPr>
                </pic:pic>
              </a:graphicData>
            </a:graphic>
          </wp:inline>
        </w:drawing>
      </w:r>
    </w:p>
  </w:body>
</w:document>'''


def para(para_id, text):
    return (f'<w:p w14:paraId="{para_id}"><w:r><w:t xml:space="preserve">'
            f'{text}</w:t></w:r></w:p>')


COMMENTS = f'''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:comments xmlns:w="{W}" xmlns:w14="{W14}" xmlns:mc="{MC}" mc:Ignorable="w14">
  <w:comment w:id="0" w:author="Reviewer A" w:initials="RA" w:date="2024-01-01T09:00:00Z">
    {para("AAAA0001", "Thread A root: please clarify this phrase.")}
  </w:comment>
  <w:comment w:id="1" w:author="Reviewer B" w:initials="RB" w:date="2024-01-01T10:00:00Z">
    {para("AAAA0002", "Thread A reply: agreed, rewording now.")}
  </w:comment>
  <w:comment w:id="2" w:author="Reviewer B" w:initials="RB" w:date="2024-01-02T11:30:00Z">
    {para("BBBB0001", "Thread B root: separate thread on the same range.")}
  </w:comment>
  <w:comment w:id="3" w:author="Reviewer A" w:initials="RA" w:date="2024-01-03T08:15:00Z">
    {para("CCCC0001", "Resolved thread: this was fixed in revision two.")}
  </w:comment>
  <w:comment w:id="4" w:author="Reviewer A" w:initials="RA" w:date="2024-01-04T12:00:00Z">
    {para("DDDD0001", "First paragraph of a multi-paragraph comment body.")}
    {para("DDDD0002", "Second paragraph, still the same comment.")}
    {para("DDDD0003", "Third and final paragraph.")}
  </w:comment>
</w:comments>'''

COMMENTS_EXTENDED = f'''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w15:commentsEx xmlns:w15="{W15}" xmlns:mc="{MC}" mc:Ignorable="w15">
  <w15:commentEx w15:paraId="AAAA0001" w15:done="0"/>
  <w15:commentEx w15:paraId="AAAA0002" w15:paraIdParent="AAAA0001" w15:done="0"/>
  <w15:commentEx w15:paraId="BBBB0001" w15:done="0"/>
  <w15:commentEx w15:paraId="CCCC0001" w15:done="1"/>
  <w15:commentEx w15:paraId="DDDD0003" w15:done="0"/>
</w15:commentsEx>'''

COMMENTS_IDS = f'''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w16cid:commentsIds xmlns:w16cid="{W16CID}" xmlns:mc="{MC}" mc:Ignorable="w16cid">
  <w16cid:commentId w16cid:paraId="AAAA0001" w16cid:durableId="10000000"/>
  <w16cid:commentId w16cid:paraId="AAAA0002" w16cid:durableId="10000001"/>
  <w16cid:commentId w16cid:paraId="BBBB0001" w16cid:durableId="10000002"/>
  <w16cid:commentId w16cid:paraId="CCCC0001" w16cid:durableId="10000003"/>
  <w16cid:commentId w16cid:paraId="DDDD0003" w16cid:durableId="10000004"/>
</w16cid:commentsIds>'''

COMMENTS_EXTENSIBLE = f'''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w16cex:commentsExtensible xmlns:w16cex="{W16CEX}" xmlns:mc="{MC}" mc:Ignorable="w16cex">
  <w16cex:commentExtensible w16cex:durableId="10000000" w16cex:dateUtc="2024-01-01T09:00:00Z"/>
  <w16cex:commentExtensible w16cex:durableId="10000001" w16cex:dateUtc="2024-01-01T10:00:00Z"/>
  <w16cex:commentExtensible w16cex:durableId="10000002" w16cex:dateUtc="2024-01-02T11:30:00Z"/>
  <w16cex:commentExtensible w16cex:durableId="10000003" w16cex:dateUtc="2024-01-03T08:15:00Z"/>
  <w16cex:commentExtensible w16cex:durableId="10000004" w16cex:dateUtc="2024-01-04T17:45:30.5Z"/>
</w16cex:commentsExtensible>'''

def one_pixel_png():
    raw = b'\x00\x00\x00\x00\x00'
    idat = zlib.compress(raw, 9)

    def chunk(ctype, data):
        crc = zlib.crc32(ctype + data) & 0xFFFFFFFF
        return struct.pack('>I', len(data)) + ctype + data + struct.pack('>I', crc)

    ihdr = struct.pack('>IIBBBBB', 1, 1, 8, 6, 0, 0, 0)
    return b'\x89PNG\r\n\x1a\n' + chunk(b'IHDR', ihdr) + chunk(b'IDAT', idat) \
        + chunk(b'IEND', b'')


ENTRIES = [
    ('[Content_Types].xml', CONTENT_TYPES),
    ('_rels/.rels', RELS),
    ('word/document.xml', DOCUMENT),
    ('word/_rels/document.xml.rels', DOC_RELS),
    ('word/comments.xml', COMMENTS),
    ('word/commentsExtended.xml', COMMENTS_EXTENDED),
    ('word/commentsIds.xml', COMMENTS_IDS),
    ('word/commentsExtensible.xml', COMMENTS_EXTENSIBLE),
    ('word/media/image1.png', one_pixel_png()),
]


def build(path):
    p = pathlib.Path(path)
    p.parent.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(p, 'w', zipfile.ZIP_DEFLATED) as z:
        for name, data in ENTRIES:
            info = zipfile.ZipInfo(name, date_time=(1980, 1, 1, 0, 0, 0))
            info.compress_type = zipfile.ZIP_DEFLATED
            info.external_attr = 0o600 << 16
            z.writestr(info, data)
    return p


if __name__ == '__main__':
    out = sys.argv[1] if len(sys.argv) > 1 else 'comments.docx'
    print(build(out))
