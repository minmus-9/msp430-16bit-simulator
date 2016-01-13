########################################################################
### mip.py -- mini-IP
###

import struct

### byte 0
VERSION_MASK   = 0xf0
VERSION        = 0x10
IHL_MASK       = 0x0f

### byte 1
FLAG_DONT_FRAG = 0x80
FLAG_MORE_FRAG = 0x40
FLAG_SECURE    = 0x20
FLAG_ENCRYPT   = 0x10

FLAG_MASK      = FLAG_DONT_FRAG | FLAG_MORE_FRAG | FLAG_SECURE | FLAG_ENCRYPT
KEY_MASK       = 0x03
FLAG_KEY_MASK  = FLAG_MASK | KEY_MASK

### bytes 2-3
PROTO_MASK     = 0xf000
LENGTH_MASK    = 0x0fff

BYTE_MASK = 0xff
LONG_MASK = 0xffffffffL

class NetIF:
    def __init__(self, dst):
        self._fctr = 0L
        self._dst  = dst

    def fctr(self):
        ret = self._fctr
        self._fctr = ret + 1
        return ret

    def mtu(self):
        return self.MTU

    def sendfrag(self, frag):
        assert len(frag) <= self.mtu()
        self._dst(frag)

def _mip_send(netif,
              proto,
              df, mf, sec, enc, key,
              fctr,
              sa,
              da,
              fofs,
              ttl,
              pld,
              mac=None):
    ihl = 5
    if sec:
        ihl += 2
        assert isinstance(mac, str) and (len(mac) == 8)
    else:
        assert mac is None
    vl = VERSION | ihl
    flags = (df  and FLAG_DONT_FRAG or 0) | \
            (mf  and FLAG_MORE_FRAG or 0) | \
            (sec and FLAG_SECURE    or 0) | \
            (enc and FLAG_ENCRYPT   or 0)
    assert (not df) or (not fofs)
    assert (key & KEY_MASK) == key
    fk = flags | key
    dl = ihl + len(pld)
    assert (proto &  PROTO_MASK) == proto
    assert (dl    & LENGTH_MASK) == dl
    pl = proto | dl
    for x in (fctr, sa, da):
        assert (x & LONG_MASK) == x
    for x in (fofs, ttl):
        assert (x & BYTE_MASK) == x
    wl = [(vl << 8) | fk,
          pl,
          fctr >> 16,
          fctr & 0xffff,
          sa >> 16,
          sa & 0xffff,
          da >> 16,
          da & 0xffff,
          (fofs << 8) | ttl
          0]
    if mac:
        wl += list(struct.unpack("HHHH", mac))
    cs = 0
    for w in wl:
        cs += 0xffff ^ w
    cs ^= 0xffff
    head = struct.pack("BBHLLLBBH",
                       vl, fk, pl, fctr, sa, da, fofs, ttl, cs)
    if mac:
        head += mac
    frame = head + pld
    assert len(frame) <= netif.mtu()
    netif.sendfrag(frame)

def mip_send(netif, proto, sa, da, ttl, pld,
             df=False, sec=False, enc=False, key=None):
    assert not enc
    assert not sec
    if sec:
        assert key is not None
    else:
        assert key is None
    mtu = netif.mtu()
    ihl = (sec and 7 or 5) << 2
    tl  = ihl + len(pld)
    assert (not df) or (tl <= mtu)
    if tl <= mtu:
        _mip_send(netif,
                  proto,
                  0, 0, sec, enc, key,
                  netif.fctr(),
                  sa,
                  da,
                  0,
                  ttl,
                  pld,
                  None)
        return
    ### gotta fragment
    fctr = netif.fctr()
    assert 0

### EOF mip.py

