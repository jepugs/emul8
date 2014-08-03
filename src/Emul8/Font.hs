module Emul8.Font where

import Common.Core


-- | The bitmap font included in the CHIP-8 runtime
font :: [Byte]
font = char0 ++ char1 ++ char2 ++ char3 ++ char4 ++ char5 ++ char6 ++ char7 ++
       char8 ++ char9 ++ charA ++ charB ++ charC ++ charD ++ charE ++ charF

-- | The length of each font character in bytes
charLen = 5

-- | The length of the font in bytes
fontLen = 0x10 * charLen

-- | The position of each character relative to the start of the font, e.g. the
-- character for 'c' starts at charPosns !! 0xc
charPosns = [ charLen * i | i <- [0..0xf] ] :: [Addr]

char0 = [ 0xf0
        , 0x90
        , 0x90
        , 0x90
        , 0xf0
        ]

char1 = [ 0x20
        , 0x60
        , 0x20
        , 0x20
        , 0x70
        ]

char2 = [ 0xf0
        , 0x10
        , 0xf0
        , 0x80
        , 0xf0
        ]

char3 = [ 0xf0
        , 0x10
        , 0xf0
        , 0x10
        , 0xf0
        ]

char4 = [ 0x90
        , 0x90
        , 0xf0
        , 0x10
        , 0x10
        ]

char5 = [ 0xf0
        , 0x80
        , 0xf0
        , 0x10
        , 0xf0
        ]

char6 = [ 0xf0
        , 0x80
        , 0xf0
        , 0x90
        , 0xf0
        ]

char7 = [ 0xf0
        , 0x10
        , 0x20
        , 0x40
        , 0x40
        ]

char8 = [ 0xf0
        , 0x90
        , 0xf0
        , 0x90
        , 0xf0
        ]

char9 = [ 0xf0
        , 0x90
        , 0xf0
        , 0x10
        , 0xf0
        ]

charA = [ 0xf0
        , 0x90
        , 0xf0
        , 0x90
        , 0x90
        ]

charB = [ 0xe0
        , 0x90
        , 0xe0
        , 0x90
        , 0xe0
        ]

charC = [ 0xf0
        , 0x80
        , 0x80
        , 0x80
        , 0xf0
        ]

charD = [ 0xe0
        , 0x90
        , 0x90
        , 0x90
        , 0xe0
        ]

charE = [ 0xf0
        , 0x80
        , 0xf0
        , 0x80
        , 0xf0
        ]

charF = [ 0xf0
        , 0x80
        , 0xf0
        , 0x80
        , 0x80
        ]
