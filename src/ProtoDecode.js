"use strict"

var TWO_TO_32 = 4294967296
var TWO_TO_52 = 4503599627370496

exports.joinFloat64 = bitsLow => bitsHigh => {
  var sign = ((bitsHigh >> 31) * 2 + 1)
  var exp = (bitsHigh >>> 20) & 0x7FF
  var mant = TWO_TO_32 * (bitsHigh & 0xFFFFF) + bitsLow

  if (exp == 0x7FF) {
    if (mant) {
      return NaN
    } else {
      return sign * Infinity
    }
  }

  if (exp == 0) {
    // Denormal.
    return sign * Math.pow(2, -1074) * mant
  } else {
    return sign * Math.pow(2, exp - 1075) *
            (mant + TWO_TO_52)
  }
}

const joinUint64 = (bitsLow, bitsHigh) => {
  return bitsHigh * TWO_TO_32 + (bitsLow >>> 0);
}

exports.joinInt64 = bitsLow => bitsHigh => {
  // If the high bit is set, do a manual two's complement conversion.
  var sign = (bitsHigh & 0x80000000)
  if (sign) {
    bitsLow = (~bitsLow + 1) >>> 0
    bitsHigh = ~bitsHigh >>> 0
    if (bitsLow == 0) {
      bitsHigh = (bitsHigh + 1) >>> 0
    }
  }

  var result = joinUint64(bitsLow, bitsHigh)
  return sign ? -result : result
}

exports.readSplitVarint64 = bytes => pos => success => failure => {
  var temp = 128
  var lowBits = 0
  var highBits = 0

  // Read the first four bytes of the varint, stopping at the terminator if we
  // see it.
  for (var i = 0; i < 4 && temp >= 128; i++) {
    temp = bytes[pos++];
    lowBits |= (temp & 0x7F) << (i * 7);
  }

  if (temp >= 128) {
    // Read the fifth byte, which straddles the low and high dwords.
    temp = bytes[pos++];
    lowBits |= (temp & 0x7F) << 28;
    highBits |= (temp & 0x7F) >> 4;
  }

  if (temp >= 128) {
    // Read the sixth through tenth byte.
    for (var i = 0; i < 5 && temp >= 128; i++) {
      temp = bytes[pos++];
      highBits |= (temp & 0x7F) << (i * 7 + 3);
    }
  }

  if (temp < 128) {
    return success({ pos: pos, val: { low: lowBits >>> 0, high: highBits >>> 0 }})
  } else {
    return failure('Failed to read varint, encoding is invalid.')
  }
}