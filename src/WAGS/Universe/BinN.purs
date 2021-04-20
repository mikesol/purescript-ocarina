module WAGS.Universe.BinN where

import Type.Function (type ($))
import WAGS.Universe.Bin (Bc, Bn, I, O)

-- for x in range(99): print('-- | Alias for %d in binary\ntype D%d' % (x,x) + ' = ' + (' '.join(["Bc %s $ " % ('O' if x == '0' else 'I') for x in "{0:b}".format(x)[::-1]])+' Bn'))
-- | Alias for 0 in binary
type D0
  = Bc O $ Bn

-- | Alias for 1 in binary
type D1
  = Bc I $ Bn

-- | Alias for 2 in binary
type D2
  = Bc O $ Bc I $ Bn

-- | Alias for 3 in binary
type D3
  = Bc I $ Bc I $ Bn

-- | Alias for 4 in binary
type D4
  = Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 5 in binary
type D5
  = Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 6 in binary
type D6
  = Bc O $ Bc I $ Bc I $ Bn

-- | Alias for 7 in binary
type D7
  = Bc I $ Bc I $ Bc I $ Bn

-- | Alias for 8 in binary
type D8
  = Bc O $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 9 in binary
type D9
  = Bc I $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 10 in binary
type D10
  = Bc O $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 11 in binary
type D11
  = Bc I $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 12 in binary
type D12
  = Bc O $ Bc O $ Bc I $ Bc I $ Bn

-- | Alias for 13 in binary
type D13
  = Bc I $ Bc O $ Bc I $ Bc I $ Bn

-- | Alias for 14 in binary
type D14
  = Bc O $ Bc I $ Bc I $ Bc I $ Bn

-- | Alias for 15 in binary
type D15
  = Bc I $ Bc I $ Bc I $ Bc I $ Bn

-- | Alias for 16 in binary
type D16
  = Bc O $ Bc O $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 17 in binary
type D17
  = Bc I $ Bc O $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 18 in binary
type D18
  = Bc O $ Bc I $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 19 in binary
type D19
  = Bc I $ Bc I $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 20 in binary
type D20
  = Bc O $ Bc O $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 21 in binary
type D21
  = Bc I $ Bc O $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 22 in binary
type D22
  = Bc O $ Bc I $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 23 in binary
type D23
  = Bc I $ Bc I $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 24 in binary
type D24
  = Bc O $ Bc O $ Bc O $ Bc I $ Bc I $ Bn

-- | Alias for 25 in binary
type D25
  = Bc I $ Bc O $ Bc O $ Bc I $ Bc I $ Bn

-- | Alias for 26 in binary
type D26
  = Bc O $ Bc I $ Bc O $ Bc I $ Bc I $ Bn

-- | Alias for 27 in binary
type D27
  = Bc I $ Bc I $ Bc O $ Bc I $ Bc I $ Bn

-- | Alias for 28 in binary
type D28
  = Bc O $ Bc O $ Bc I $ Bc I $ Bc I $ Bn

-- | Alias for 29 in binary
type D29
  = Bc I $ Bc O $ Bc I $ Bc I $ Bc I $ Bn

-- | Alias for 30 in binary
type D30
  = Bc O $ Bc I $ Bc I $ Bc I $ Bc I $ Bn

-- | Alias for 31 in binary
type D31
  = Bc I $ Bc I $ Bc I $ Bc I $ Bc I $ Bn

-- | Alias for 32 in binary
type D32
  = Bc O $ Bc O $ Bc O $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 33 in binary
type D33
  = Bc I $ Bc O $ Bc O $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 34 in binary
type D34
  = Bc O $ Bc I $ Bc O $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 35 in binary
type D35
  = Bc I $ Bc I $ Bc O $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 36 in binary
type D36
  = Bc O $ Bc O $ Bc I $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 37 in binary
type D37
  = Bc I $ Bc O $ Bc I $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 38 in binary
type D38
  = Bc O $ Bc I $ Bc I $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 39 in binary
type D39
  = Bc I $ Bc I $ Bc I $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 40 in binary
type D40
  = Bc O $ Bc O $ Bc O $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 41 in binary
type D41
  = Bc I $ Bc O $ Bc O $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 42 in binary
type D42
  = Bc O $ Bc I $ Bc O $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 43 in binary
type D43
  = Bc I $ Bc I $ Bc O $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 44 in binary
type D44
  = Bc O $ Bc O $ Bc I $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 45 in binary
type D45
  = Bc I $ Bc O $ Bc I $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 46 in binary
type D46
  = Bc O $ Bc I $ Bc I $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 47 in binary
type D47
  = Bc I $ Bc I $ Bc I $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 48 in binary
type D48
  = Bc O $ Bc O $ Bc O $ Bc O $ Bc I $ Bc I $ Bn

-- | Alias for 49 in binary
type D49
  = Bc I $ Bc O $ Bc O $ Bc O $ Bc I $ Bc I $ Bn

-- | Alias for 50 in binary
type D50
  = Bc O $ Bc I $ Bc O $ Bc O $ Bc I $ Bc I $ Bn

-- | Alias for 51 in binary
type D51
  = Bc I $ Bc I $ Bc O $ Bc O $ Bc I $ Bc I $ Bn

-- | Alias for 52 in binary
type D52
  = Bc O $ Bc O $ Bc I $ Bc O $ Bc I $ Bc I $ Bn

-- | Alias for 53 in binary
type D53
  = Bc I $ Bc O $ Bc I $ Bc O $ Bc I $ Bc I $ Bn

-- | Alias for 54 in binary
type D54
  = Bc O $ Bc I $ Bc I $ Bc O $ Bc I $ Bc I $ Bn

-- | Alias for 55 in binary
type D55
  = Bc I $ Bc I $ Bc I $ Bc O $ Bc I $ Bc I $ Bn

-- | Alias for 56 in binary
type D56
  = Bc O $ Bc O $ Bc O $ Bc I $ Bc I $ Bc I $ Bn

-- | Alias for 57 in binary
type D57
  = Bc I $ Bc O $ Bc O $ Bc I $ Bc I $ Bc I $ Bn

-- | Alias for 58 in binary
type D58
  = Bc O $ Bc I $ Bc O $ Bc I $ Bc I $ Bc I $ Bn

-- | Alias for 59 in binary
type D59
  = Bc I $ Bc I $ Bc O $ Bc I $ Bc I $ Bc I $ Bn

-- | Alias for 60 in binary
type D60
  = Bc O $ Bc O $ Bc I $ Bc I $ Bc I $ Bc I $ Bn

-- | Alias for 61 in binary
type D61
  = Bc I $ Bc O $ Bc I $ Bc I $ Bc I $ Bc I $ Bn

-- | Alias for 62 in binary
type D62
  = Bc O $ Bc I $ Bc I $ Bc I $ Bc I $ Bc I $ Bn

-- | Alias for 63 in binary
type D63
  = Bc I $ Bc I $ Bc I $ Bc I $ Bc I $ Bc I $ Bn

-- | Alias for 64 in binary
type D64
  = Bc O $ Bc O $ Bc O $ Bc O $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 65 in binary
type D65
  = Bc I $ Bc O $ Bc O $ Bc O $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 66 in binary
type D66
  = Bc O $ Bc I $ Bc O $ Bc O $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 67 in binary
type D67
  = Bc I $ Bc I $ Bc O $ Bc O $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 68 in binary
type D68
  = Bc O $ Bc O $ Bc I $ Bc O $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 69 in binary
type D69
  = Bc I $ Bc O $ Bc I $ Bc O $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 70 in binary
type D70
  = Bc O $ Bc I $ Bc I $ Bc O $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 71 in binary
type D71
  = Bc I $ Bc I $ Bc I $ Bc O $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 72 in binary
type D72
  = Bc O $ Bc O $ Bc O $ Bc I $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 73 in binary
type D73
  = Bc I $ Bc O $ Bc O $ Bc I $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 74 in binary
type D74
  = Bc O $ Bc I $ Bc O $ Bc I $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 75 in binary
type D75
  = Bc I $ Bc I $ Bc O $ Bc I $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 76 in binary
type D76
  = Bc O $ Bc O $ Bc I $ Bc I $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 77 in binary
type D77
  = Bc I $ Bc O $ Bc I $ Bc I $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 78 in binary
type D78
  = Bc O $ Bc I $ Bc I $ Bc I $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 79 in binary
type D79
  = Bc I $ Bc I $ Bc I $ Bc I $ Bc O $ Bc O $ Bc I $ Bn

-- | Alias for 80 in binary
type D80
  = Bc O $ Bc O $ Bc O $ Bc O $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 81 in binary
type D81
  = Bc I $ Bc O $ Bc O $ Bc O $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 82 in binary
type D82
  = Bc O $ Bc I $ Bc O $ Bc O $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 83 in binary
type D83
  = Bc I $ Bc I $ Bc O $ Bc O $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 84 in binary
type D84
  = Bc O $ Bc O $ Bc I $ Bc O $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 85 in binary
type D85
  = Bc I $ Bc O $ Bc I $ Bc O $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 86 in binary
type D86
  = Bc O $ Bc I $ Bc I $ Bc O $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 87 in binary
type D87
  = Bc I $ Bc I $ Bc I $ Bc O $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 88 in binary
type D88
  = Bc O $ Bc O $ Bc O $ Bc I $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 89 in binary
type D89
  = Bc I $ Bc O $ Bc O $ Bc I $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 90 in binary
type D90
  = Bc O $ Bc I $ Bc O $ Bc I $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 91 in binary
type D91
  = Bc I $ Bc I $ Bc O $ Bc I $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 92 in binary
type D92
  = Bc O $ Bc O $ Bc I $ Bc I $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 93 in binary
type D93
  = Bc I $ Bc O $ Bc I $ Bc I $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 94 in binary
type D94
  = Bc O $ Bc I $ Bc I $ Bc I $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 95 in binary
type D95
  = Bc I $ Bc I $ Bc I $ Bc I $ Bc I $ Bc O $ Bc I $ Bn

-- | Alias for 96 in binary
type D96
  = Bc O $ Bc O $ Bc O $ Bc O $ Bc O $ Bc I $ Bc I $ Bn

-- | Alias for 97 in binary
type D97
  = Bc I $ Bc O $ Bc O $ Bc O $ Bc O $ Bc I $ Bc I $ Bn

-- | Alias for 98 in binary
type D98
  = Bc O $ Bc I $ Bc O $ Bc O $ Bc O $ Bc I $ Bc I $ Bn
