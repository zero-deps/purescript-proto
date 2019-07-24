"use strict"

var TWO_TO_32 = 4294967296
var TWO_TO_52 = 4503599627370496

exports.joinFloat64 = function(bitsLow) {
  return function(bitsHigh) {
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
}
