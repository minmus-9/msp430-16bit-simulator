########################################################################
### micd.py -- mote ICD
###

__doc__ = r"""
Terminology:
  GW = Gateway (mote network collection point)
  GO = Gateway-originated
  GT = Gateway-terminated
  SD = System device; any device capable of communicating
       with the system via the GSS and/or SBD

Byte Ordering:
  Big endian

Mote Address (MA) -- 1 or 2 bytes:
  0bbbbbbb          -  7-bit mote address (  0 to    127)
  1bbbbbbb bbbbbbbb - 15-bit mote address (128 to 32,767)

  Suggest that
    NMAs use 0-127        shortest addr, most prevalent
    NMAs use 128-16383    for large deployments
    GMAs use 16384-24575  low prevalence
    TMAs use 24576-32767  low prevalence

Data Lengths -- 1 or 2 bytes:
  0bbbbbbb          -   0 to    127
  1bbbbbbb bbbbbbbb - 128 to 32,767

Information Element Identifier (IEI) -- 1 byte:
  1 byte, range 0 to 255

Information Element (IE) -- 2 to 32,770 bytes:
  IEI    - 1 byte, as above
  Length - 1 or 2 bytes, as above
  Body   - Size as in Length field; IEI dictates interpretation

Message Version -- 1 byte:
  1 byte, value must be "1" (ASCII 49 0x31)

  Chosen this way to avoid confusion with the DirectIP ICD.

Transaction ID (XID) -- 1 to 4 bytes:

  00bbbbbb                            -         0 to            63
  01bbbbbb bbbbbbbb                   -        64 to        16,383
  10bbbbbb bbbbbbbb bbbbbbbb          -    16,384 to     4,194,303
  11bbbbbb bbbbbbbb bbbbbbbb bbbbbbbb - 4,194,303 to 1,073,741,823

  An integer encoded as above. Every SD maintains its own
  copy of its one-up XID counter. For every message, the
  originator includes its XID in the message header.
  
IMEI -- 15 bytes:
  15 ASCII bytes in the range "0" through "9" (48-57, 0x30-0x39)

  The "default" IMEI is "000000000000000" (fifteen "0" characters).
  This address is typically used as a destination address for GO
  messages (like a default route in IP).

IP Address (IP) -- 15 bytes:
  15-byte ASCII IPv4 address in decimal "dotted quad" notation
  with zero padding. The "." character is ASCII 46 (0x2e).

  Examples:
    216.109.112.135 -- yahoo.com
    024.248.073.200 -- cox.com -- note zero padding of octets

  Interpretation of this format is left up to a particular
  system implementation.

System Address (SA) -- 15 bytes:
  15-byte IMEI or IP, as above

  The two formats can be distinguished based on whether or not
  any "." characters are present.

Key -- 1 byte:
  1-byte key identifier in range 0-255

  Interpretation of this field is left to a particular system
  implementation.

Location -- 9 to 12 bytes:
  LatDeg - 1 byte:

             sddddddd

           s = sign of latitude (1 = negative [South])
           d = latitude in degrees 0 to 90
  LatMin - 2 bytes, thousandths of degrees 0 to 59,999
  LonDeg - 1 byte, 0 to 180
  LonMin - 2 bytes, thousandths of degrees 0 to 59,999
  Alt    - 2 bytes:
  
             snaaaaaa aaaaaaaa

           s = sign of LonDeg above (1 = negative [West])
           n = sign of altitude (1 = negative)
           a = altitude in meters 0 to 16383
  CEP    - 1-4 bytes as in XID above, CEP in meters, 0=unknown

Message Flags -- 1 byte:
  0x01 - Ack requested? (set if sender wants an ack)
  0x02 - Ack bit (set iff this is an ack to a previous message)
           Length field (see below) must be zero and payload
           will be empty.
  0x04 - Encrypted? If set, IEI 0x02 must be present and all IEs
         following IE 0x02 will be encrypted. If clear, IEI 0x02
         may be present (MAC-only operation) or absent (unsecured
         operation).

  Other bits must be set to zero in version "1"

Quality -- 1 byte:
  1 byte, range 0-255

Neighbor -- 2 or 3 bytes:
  MA   -- 1 or 2 byte mote address
  Qual -- 1 byte RF Quality

Time -- 4 bytes:
  4 byte 32-bit integer, seconds since 1 Jan 2007 00:00:00 UTC

  This epoch is 1,167,609,600 seconds after 1 Jan 1970 00:00:00 UTC

Message -- 2 to 32,770 bytes:
  Version - 1 byte, as above
  Length  - 1 or 2 bytes, as above
  Payload - Size as in Length field; a sequence of IEs,
            in *ASCENDING* order by IEI

########################################################################
IEI 0x00 -- 3 to 6 bytes -- REQUIRED message metadata
  Flags   - 1 byte, as above
  XID     - 1 to 4 bytes, as above
  Time    - XXX

IEI 0x01 -- 17 bytes -- REQUIRED source address
  * Length must be 15 (0x0f)
  * Body format
      SA - 15 bytes

IEI 0x02 -- 11 bytes -- OPTIONAL encryption info [0 or 1]
  * Length must be 9 (0x09)
  * Body format
      Key - 1 byte
      MAC - 8 bytes

IEI 0x10 -- 17 bytes -- OPTIONAL destination address [0 or more]
  * Length must be 15 (0x0f)
  * Body format
      SA - 15 bytes

IEI 0x20 -- 12 to 16 bytes -- OPTIONAL mote location [0 or more]
  * Length is 10 to 14
  * Body format
      MA  -- 1 or 2 byte mote address
      Loc -- 9 to 12 byte location

IEI 0x21 -- 3 or more bytes -- OPTIONAL mote health information [0 or 1]
  * Length
  * Body format
      MA     -- 1 or 2 byte address of subject mote
      Status -- 1 byte mote status XXX

IE 0x22 -- 6 to 29 bytes -- OPTIONAL mote network info [0 or more]
  * Body format
      MA       -- 1 or 2 byte address of subject mote
      Cnt      -- 1 byte count of its neighbors
      Neighbor -- 2 or 3 byte Neighbor entry
      Neighbor -- 2 or 3 byte Neighbor entry
      ...

    Up to eight Neighbor entries will be included; i.e., if
    Cnt is 10, only the first 8 entries will be reported.

IE 0xff -- 6 bytes -- OPTIONAL checksum [0 or 1]
  * Length must be 4
  * Body format
      CRC32 -- 4 bytes
"""



### EOF micd.py

