-- Much of this module was adapted from
-- <https://github.com/weswigham/simplex/tree/master/haskell wesigham's Simplex Noise implemenation>
module ChaosBox.Noise
  ( noise
  , newNoise
  , noise2
  , newNoise2
  , noise3
  , newNoise3
  , noise4
  , newNoise4
  )
where

import           Prelude              hiding (init)

import           ChaosBox.Generate
import           ChaosBox.Geometry.P2
import           Control.Monad.Random
import           Data.Vector          (Vector, init, (!))
import qualified Data.Vector          as V
import           Linear

-- | One dimensional simplex noise
noise :: Double -> Double
noise x = noise2D x 0

-- | Generate one dimensional simplex noise with a random seed.
newNoise :: Generate (Double -> Double)
newNoise = do
  seed <- getRandom
  pure $ \x -> noise (x + seed)

-- | Two dimensional simplex noise
noise2 :: P2 -> Double
noise2 (V2 x y) = noise2D x y

-- | Generate two dimensional simplex noise with a random seed.
newNoise2 :: Generate (P2 -> Double)
newNoise2 = do
  seed <- V2 <$> getRandom <*> getRandom
  pure $ \x -> noise2 (x + seed)

-- | Three dimensional simplex noise
noise3 :: V3 Double -> Double
noise3 (V3 x y z) = noise3D x y z

-- | Generate three dimensional simplex noise with a random seed.
newNoise3 :: Generate (V3 Double -> Double)
newNoise3 = do
  seed <- V3 <$> getRandom <*> getRandom <*> getRandom
  pure $ \x -> noise3 (x + seed)

-- | Four dimensional simplex noise
noise4 :: V4 Double -> Double
noise4 (V4 x y z w) = noise4D x y z w

-- | Generate four dimensional simplex noise with a random seed.
newNoise4 :: Generate (V4 Double -> Double)
newNoise4 = do
  seed <- V4 <$> getRandom <*> getRandom <*> getRandom <*> getRandom
  pure $ \x -> noise4 (x + seed)

vector2 :: [[a]] -> Vector (Vector a)
vector2 = V.fromList . fmap V.fromList

gradients3d :: Vector (Vector Double)
gradients3d = vector2
  [ [1, 1, 0]
  , [-1, 1, 0]
  , [1, -1, 0]
  , [-1, -1, 0]
  , [1, 0, 1]
  , [-1, 0, 1]
  , [1, 0, -1]
  , [-1, 0, -1]
  , [0, 1, 1]
  , [0, -1, 1]
  , [0, 1, -1]
  , [0, -1, -1]
  ]

gradients4d :: Vector (Vector Double)
gradients4d = vector2
  [ [0, 1, 1, 1]
  , [0, 1, 1, -1]
  , [0, 1, -1, 1]
  , [0, 1, -1, -1]
  , [0, -1, 1, 1]
  , [0, -1, 1, -1]
  , [0, -1, -1, 1]
  , [0, -1, -1, -1]
  , [1, 0, 1, 1]
  , [1, 0, 1, -1]
  , [1, 0, -1, 1]
  , [1, 0, -1, -1]
  , [-1, 0, 1, 1]
  , [-1, 0, 1, -1]
  , [-1, 0, -1, 1]
  , [-1, 0, -1, -1]
  , [1, 1, 0, 1]
  , [1, 1, 0, -1]
  , [1, -1, 0, 1]
  , [1, -1, 0, -1]
  , [-1, 1, 0, 1]
  , [-1, 1, 0, -1]
  , [-1, -1, 0, 1]
  , [-1, -1, 0, -1]
  , [1, 1, 1, 0]
  , [1, 1, -1, 0]
  , [1, -1, 1, 0]
  , [1, -1, -1, 0]
  , [-1, 1, 1, 0]
  , [-1, 1, -1, 0]
  , [-1, -1, 1, 0]
  , [-1, -1, -1, 0]
  ]


perm :: Vector Int
perm = V.fromList
  [ 151
  , 160
  , 137
  , 91
  , 90
  , 15
  , 131
  , 13
  , 201
  , 95
  , 96
  , 53
  , 194
  , 233
  , 7
  , 225
  , 140
  , 36
  , 103
  , 30
  , 69
  , 142
  , 8
  , 99
  , 37
  , 240
  , 21
  , 10
  , 23
  , 190
  , 6
  , 148
  , 247
  , 120
  , 234
  , 75
  , 0
  , 26
  , 197
  , 62
  , 94
  , 252
  , 219
  , 203
  , 117
  , 35
  , 11
  , 32
  , 57
  , 177
  , 33
  , 88
  , 237
  , 149
  , 56
  , 87
  , 174
  , 20
  , 125
  , 136
  , 171
  , 168
  , 68
  , 175
  , 74
  , 165
  , 71
  , 134
  , 139
  , 48
  , 27
  , 166
  , 77
  , 146
  , 158
  , 231
  , 83
  , 111
  , 229
  , 122
  , 60
  , 211
  , 133
  , 230
  , 220
  , 105
  , 92
  , 41
  , 55
  , 46
  , 245
  , 40
  , 244
  , 102
  , 143
  , 54
  , 65
  , 25
  , 63
  , 161
  , 1
  , 216
  , 80
  , 73
  , 209
  , 76
  , 132
  , 187
  , 208
  , 89
  , 18
  , 169
  , 200
  , 196
  , 135
  , 130
  , 116
  , 188
  , 159
  , 86
  , 164
  , 100
  , 109
  , 198
  , 173
  , 186
  , 3
  , 64
  , 52
  , 217
  , 226
  , 250
  , 124
  , 123
  , 5
  , 202
  , 38
  , 147
  , 118
  , 126
  , 255
  , 82
  , 85
  , 212
  , 207
  , 206
  , 59
  , 227
  , 47
  , 16
  , 58
  , 17
  , 182
  , 189
  , 28
  , 42
  , 223
  , 183
  , 170
  , 213
  , 119
  , 248
  , 152
  , 2
  , 44
  , 154
  , 163
  , 70
  , 221
  , 153
  , 101
  , 155
  , 167
  , 43
  , 172
  , 9
  , 129
  , 22
  , 39
  , 253
  , 19
  , 98
  , 108
  , 110
  , 79
  , 113
  , 224
  , 232
  , 178
  , 185
  , 112
  , 104
  , 218
  , 246
  , 97
  , 228
  , 251
  , 34
  , 242
  , 193
  , 238
  , 210
  , 144
  , 12
  , 191
  , 179
  , 162
  , 241
  , 81
  , 51
  , 145
  , 235
  , 249
  , 14
  , 239
  , 107
  , 49
  , 192
  , 214
  , 31
  , 181
  , 199
  , 106
  , 157
  , 184
  , 84
  , 204
  , 176
  , 115
  , 121
  , 50
  , 45
  , 127
  , 4
  , 150
  , 254
  , 138
  , 236
  , 205
  , 93
  , 222
  , 114
  , 67
  , 29
  , 24
  , 72
  , 243
  , 141
  , 128
  , 195
  , 78
  , 66
  , 215
  , 61
  , 156
  , 180
  , 151
  , 160
  , 137
  , 91
  , 90
  , 15
  , 131
  , 13
  , 201
  , 95
  , 96
  , 53
  , 194
  , 233
  , 7
  , 225
  , 140
  , 36
  , 103
  , 30
  , 69
  , 142
  , 8
  , 99
  , 37
  , 240
  , 21
  , 10
  , 23
  , 190
  , 6
  , 148
  , 247
  , 120
  , 234
  , 75
  , 0
  , 26
  , 197
  , 62
  , 94
  , 252
  , 219
  , 203
  , 117
  , 35
  , 11
  , 32
  , 57
  , 177
  , 33
  , 88
  , 237
  , 149
  , 56
  , 87
  , 174
  , 20
  , 125
  , 136
  , 171
  , 168
  , 68
  , 175
  , 74
  , 165
  , 71
  , 134
  , 139
  , 48
  , 27
  , 166
  , 77
  , 146
  , 158
  , 231
  , 83
  , 111
  , 229
  , 122
  , 60
  , 211
  , 133
  , 230
  , 220
  , 105
  , 92
  , 41
  , 55
  , 46
  , 245
  , 40
  , 244
  , 102
  , 143
  , 54
  , 65
  , 25
  , 63
  , 161
  , 1
  , 216
  , 80
  , 73
  , 209
  , 76
  , 132
  , 187
  , 208
  , 89
  , 18
  , 169
  , 200
  , 196
  , 135
  , 130
  , 116
  , 188
  , 159
  , 86
  , 164
  , 100
  , 109
  , 198
  , 173
  , 186
  , 3
  , 64
  , 52
  , 217
  , 226
  , 250
  , 124
  , 123
  , 5
  , 202
  , 38
  , 147
  , 118
  , 126
  , 255
  , 82
  , 85
  , 212
  , 207
  , 206
  , 59
  , 227
  , 47
  , 16
  , 58
  , 17
  , 182
  , 189
  , 28
  , 42
  , 223
  , 183
  , 170
  , 213
  , 119
  , 248
  , 152
  , 2
  , 44
  , 154
  , 163
  , 70
  , 221
  , 153
  , 101
  , 155
  , 167
  , 43
  , 172
  , 9
  , 129
  , 22
  , 39
  , 253
  , 19
  , 98
  , 108
  , 110
  , 79
  , 113
  , 224
  , 232
  , 178
  , 185
  , 112
  , 104
  , 218
  , 246
  , 97
  , 228
  , 251
  , 34
  , 242
  , 193
  , 238
  , 210
  , 144
  , 12
  , 191
  , 179
  , 162
  , 241
  , 81
  , 51
  , 145
  , 235
  , 249
  , 14
  , 239
  , 107
  , 49
  , 192
  , 214
  , 31
  , 181
  , 199
  , 106
  , 157
  , 184
  , 84
  , 204
  , 176
  , 115
  , 121
  , 50
  , 45
  , 127
  , 4
  , 150
  , 254
  , 138
  , 236
  , 205
  , 93
  , 222
  , 114
  , 67
  , 29
  , 24
  , 72
  , 243
  , 141
  , 128
  , 195
  , 78
  , 66
  , 215
  , 61
  , 156
  , 180
  ]

simplex :: Vector (Vector Double)
simplex = vector2
  [ [0, 1, 2, 3]
  , [0, 1, 3, 2]
  , [0, 0, 0, 0]
  , [0, 2, 3, 1]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [1, 2, 3, 0]
  , [0, 2, 1, 3]
  , [0, 0, 0, 0]
  , [0, 3, 1, 2]
  , [0, 3, 2, 1]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [1, 3, 2, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [1, 2, 0, 3]
  , [0, 0, 0, 0]
  , [1, 3, 0, 2]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [2, 3, 0, 1]
  , [2, 3, 1, 0]
  , [1, 0, 2, 3]
  , [1, 0, 3, 2]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [2, 0, 3, 1]
  , [0, 0, 0, 0]
  , [2, 1, 3, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [2, 0, 1, 3]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [3, 0, 1, 2]
  , [3, 0, 2, 1]
  , [0, 0, 0, 0]
  , [3, 1, 2, 0]
  , [2, 1, 0, 3]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [3, 1, 0, 2]
  , [0, 0, 0, 0]
  , [3, 2, 0, 1]
  , [3, 2, 1, 0]
  ]

noise2D :: Double -> Double -> Double
noise2D x y =
    --space-skewing factors
  let
    f2  = 0.5 * ((sqrt 3) - 1)
    s   = (x + y) * f2
    i   = floor (x + s)
    j   = floor (y + s)
    g2  = ((3 - (sqrt 3)) / 6)

    --calculate the positions of the vertices of the simplex
    t   = (fromIntegral (i + j)) * g2
    x0  = x - (fromIntegral i - t)
    y0  = y - (fromIntegral j - t)

    i1  = if (x0 > y0) then 1 else 0
    j1  = if (x0 > y0) then 0 else 1

    x1  = x0 - fromIntegral i1 + g2
    y1  = y0 - fromIntegral j1 + g2
    x2  = x0 - 1 + 2 * g2
    y2  = y0 - 1 + 2 * g2

    --get the gradients at each corner from the arrays above
    ii  = i `mod` 256
    jj  = j `mod` 256

    gi0 = (perm ! (ii + (perm ! jj))) `mod` 12
    gi1 = (perm ! (ii + i1 + (perm ! (jj + j1)))) `mod` 12
    gi2 = (perm ! (ii + 1 + (perm ! (jj + 1)))) `mod` 12

    --calculate the contributions form the corners of the simplex
    t0  = 0.5 - x0 * x0 - y0 * y0
    t1  = 0.5 - x1 * x1 - y1 * y1
    t2  = 0.5 - x2 * x2 - y2 * y2

    n0  = if (t0 < 0)
      then 0
      else ((t0 ** 4) * dot (init (gradients3d ! gi0)) (V.fromList [x0, y0]))
    n1 = if (t1 < 0)
      then 0
      else ((t1 ** 4) * dot (init (gradients3d ! gi1)) (V.fromList [x1, y1]))
    n2 = if (t2 < 0)
      then 0
      else ((t2 ** 4) * dot (init (gradients3d ! gi2)) (V.fromList [x2, y2]))
  in
    70.0 * (n0 + n1 + n2) --sum the contributions

noise3D :: Double -> Double -> Double -> Double
noise3D x y z
  =
    --space skewing-factors
    let
                             f3 = 1 / 3
                             s = (x + y + z) * f3
                             i = floor (x + s)
                             j = floor (y + s)
                             k = floor (z + s)

                             g3 = 1 / 6
                             t = fromIntegral (i + j + k) * g3

                             --cell origin coordinates
                             x0 = (x - (fromIntegral i - t))
                             y0 = (y - (fromIntegral j - t))
                             z0 = (z - (fromIntegral k - t))

                             --ordering of other coordinates
                             (i1, j1, k1, i2, j2, k2) = if (x0 >= y0)
                               then if (y0 >= z0)
                                 then (1, 0, 0, 1, 1, 0)
                                 else (if (x0 >= z0) then (1, 0, 0, 1, 0, 1) else (0, 0, 1, 1, 0, 1))
                               else
                                 (if (y0 < z0)
                                   then (0, 0, 1, 0, 1, 1)
                                   else (if (x0 < z0) then (0, 1, 0, 0, 1, 1) else (0, 1, 0, 1, 1, 0))
                                 )

                             --coordinates of the other 3 vertices
                             x1  = x0 - fromIntegral i1 + g3
                             y1  = y0 - fromIntegral j1 + g3
                             z1  = z0 - fromIntegral k1 + g3

                             x2  = x0 - fromIntegral i2 + 2 * g3
                             y2  = y0 - fromIntegral j2 + 2 * g3
                             z2  = z0 - fromIntegral k2 + 2 * g3

                             x3  = x0 - 1 + 3 * g3
                             y3  = y0 - 1 + 3 * g3
                             z3  = z0 - 1 + 3 * g3

                             --locate gradient
                             ii  = i `mod` 256
                             jj  = j `mod` 256
                             kk  = k `mod` 256

                             gi0 = (perm ! (ii + (perm ! (jj + (perm ! kk))))) `mod` 12
                             gi1 =
                               (perm ! (ii + i1 + (perm ! (jj + j1 + (perm ! (kk + k1)))))) `mod` 12
                             gi2 =
                               (perm ! (ii + i2 + (perm ! (jj + j2 + (perm ! (kk + k2)))))) `mod` 12
                             gi3 = (perm ! (ii + 1 + (perm ! (jj + 1 + (perm ! (kk + 1)))))) `mod` 12

                             --contributions from each corner
                             t0  = 0.5 - x0 * x0 - y0 * y0 - z0 * z0
                             t1  = 0.5 - x1 * x1 - y1 * y1 - z1 * z1
                             t2  = 0.5 - x2 * x2 - y2 * y2 - z2 * z2
                             t3  = 0.5 - x3 * x3 - y3 * y3 - z3 * z3

                             n0  = if (t0 < 0)
                               then 0
                               else (t0 ** 4) * (gradients3d ! gi0 `dot` V.fromList ([x0, y0, z0]))
                             n1 = if (t1 < 0)
                               then 0
                               else (t1 ** 4) * (gradients3d ! gi1 `dot` V.fromList ([x1, y1, z1]))
                             n2 = if (t2 < 0)
                               then 0
                               else (t2 ** 4) * (gradients3d ! gi2 `dot` V.fromList ([x2, y2, z2]))
                             n3 = if (t3 < 0)
                               then 0
                               else (t3 ** 4) * (gradients3d ! gi3 `dot` V.fromList ([x3, y3, z3]))
                           in
                             32 * (n0 + n1 + n2 + n3)

      --sum the contributions

noise4D :: Double -> Double -> Double -> Double -> Double
noise4D x y z w =
    --coordinate skewwing/unskewwing
  let
    f4  = ((sqrt 5) - 1) / 4
    g4  = (5 - sqrt 5) / 20

    s   = (x + y + z + w) * f4
    i   = floor (x + s)
    j   = floor (y + s)
    k   = floor (z + s)
    l   = floor (w + s)

    --find first corner
    t   = fromIntegral (i + j + k + l) * g4
    x0  = x - (fromIntegral i - t)
    y0  = y - (fromIntegral j - t)
    z0  = z - (fromIntegral k - t)
    w0  = w - (fromIntegral l - t)

    --figure out corner order via comparisons and then lookup table
    c1  = if (x0 > y0) then 32 else 1
    c2  = if (x0 > z0) then 16 else 1
    c3  = if (y0 > z0) then 8 else 1
    c4  = if (x0 > w0) then 4 else 1
    c5  = if (y0 > w0) then 2 else 1
    c6  = if (z0 > w0) then 1 else 1
    c   = c1 + c2 + c3 + c4 + c5 + c6

    --the actual lookups...
    i1  = (if ((simplex ! c) ! 0 >= 3) then 1 else 0)
    j1  = (if ((simplex ! c) ! 1 >= 3) then 1 else 0)
    k1  = (if ((simplex ! c) ! 2 >= 3) then 1 else 0)
    l1  = (if ((simplex ! c) ! 3 >= 3) then 1 else 0)

    i2  = (if ((simplex ! c) ! 0 >= 2) then 1 else 0)
    j2  = (if ((simplex ! c) ! 1 >= 2) then 1 else 0)
    k2  = (if ((simplex ! c) ! 2 >= 2) then 1 else 0)
    l2  = (if ((simplex ! c) ! 3 >= 2) then 1 else 0)

    i3  = (if ((simplex ! c) ! 0 >= 1) then 1 else 0)
    j3  = (if ((simplex ! c) ! 1 >= 1) then 1 else 0)
    k3  = (if ((simplex ! c) ! 2 >= 1) then 1 else 0)
    l3  = (if ((simplex ! c) ! 3 >= 1) then 1 else 0)

    --actual coordinate calculations
    x1  = x0 - fromIntegral i1 + g4
    y1  = y0 - fromIntegral j1 + g4
    z1  = z0 - fromIntegral k1 + g4
    w1  = w0 - fromIntegral l1 + g4

    x2  = x0 - i2 + 2 * g4
    y2  = y0 - fromIntegral j2 + 2 * g4
    z2  = z0 - fromIntegral k2 + 2 * g4
    w2  = w0 - fromIntegral l2 + 2 * g4

    x3  = x0 - i3 + 3 * g4
    y3  = y0 - fromIntegral j3 + 3 * g4
    z3  = z0 - fromIntegral k3 + 3 * g4
    w3  = w0 - fromIntegral l3 + 3 * g4

    x4  = x0 - 1 + 4 * g4
    y4  = y0 - 1 + 4 * g4
    z4  = z0 - 1 + 4 * g4
    w4  = w0 - 1 + 4 * g4

    --find the gradient
    ii  = i `mod` 256
    jj  = j `mod` 256
    kk  = k `mod` 256
    ll  = l `mod` 256

    gi0 = perm ! (ii + (perm ! (jj + (perm ! (kk + (perm ! ll)))))) `mod` 32
    gi1 =
      perm
        !     ( ii
              + i1
              + (perm ! (jj + j1 + (perm ! (kk + k1 + (perm ! (ll + l1))))))
              )
        `mod` 32
    gi2 =
      perm
        !     ( ii
              + i1
              + (perm ! (jj + j2 + (perm ! (kk + k2 + (perm ! (ll + l2))))))
              )
        `mod` 32
    gi3 =
      perm
        !     ( ii
              + i1
              + (perm ! (jj + j3 + (perm ! (kk + k3 + (perm ! (ll + l3))))))
              )
        `mod` 32
    gi4 =
      perm
        ! (ii + i1 + (perm ! (jj + 1 + (perm ! (kk + 1 + (perm ! (ll + 1)))))))
        `mod` 32

    --contributions form each corner
    t0 = 0.5 - x0 * x0 - y0 * y0 - z0 * z0 - w0 * w0
    t1 = 0.5 - x1 * x1 - y1 * y1 - z1 * z1 - w1 * w1
    t2 = 0.5 - x2 * x2 - y2 * y2 - z2 * z2 - w2 * w2
    t3 = 0.5 - x3 * x3 - y3 * y3 - z3 * z3 - w3 * w3
    t4 = 0.5 - x4 * x4 - y4 * y4 - z4 * z4 - w4 * w4

    n0 = if (t0 < 0)
      then 0
      else (t0 ** 4) * ((gradients4d ! gi0) `dot` V.fromList [x0, y0, z0, w0])
    n1 = if (t1 < 0)
      then 0
      else (t1 ** 4) * ((gradients4d ! gi1) `dot` V.fromList [x1, y1, z1, w1])
    n2 = if (t2 < 0)
      then 0
      else (t2 ** 4) * ((gradients4d ! gi2) `dot` V.fromList [x2, y2, z2, w2])
    n3 = if (t3 < 0)
      then 0
      else (t3 ** 4) * ((gradients4d ! gi3) `dot` V.fromList [x3, y3, z3, w3])
    n4 = if (t4 < 0)
      then 0
      else (t4 ** 4) * ((gradients4d ! gi4) `dot` V.fromList [x4, y4, z4, w4])
  in
    27 * (n0 + n1 + n2 + n3 + n4)
