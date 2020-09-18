"use strict"

var FLOAT64_MAX = 1.7976931348623157e+308
var FLOAT64_MIN = 2.2250738585072014e-308
var TWO_TO_20 = 1048576
var TWO_TO_32 = 4294967296
var TWO_TO_52 = 4503599627370496

exports.splitFloat64 = value => {
  var sign = (value < 0) ? 1 : 0
  value = sign ? -value : value

  // Handle zeros.
  if (value === 0) {
    if ((1 / value) > 0) {
      // Positive zero.
      return { low: 0x00000000, high: 0x00000000 }
    } else {
      // Negative zero.
      return { low: 0x00000000, high: 0x80000000 }
    }
  }

  // Handle nans.
  if (isNaN(value)) {
    return { low: 0xFFFFFFFF, high: 0x7FFFFFFF }
  }

  // Handle infinities.
  if (value > FLOAT64_MAX) {
    return { low: 0, high: ((sign << 31) | (0x7FF00000)) >>> 0 }
  }

  // Handle denormals.
  if (value < FLOAT64_MIN) {
    // Number is a denormal.
    var mant = value / Math.pow(2, -1074)
    var mantHigh = (mant / TWO_TO_32)
    return { low: (mant >>> 0), high: ((sign << 31) | mantHigh) >>> 0 }
  }

  var exp = Math.floor(Math.log(value) / Math.LN2)
  if (exp == 1024) exp = 1023
  var mant = value * Math.pow(2, -exp)

  var mantHigh = (mant * TWO_TO_20) & 0xFFFFF
  var mantLow = (mant * TWO_TO_52) >>> 0

  return { low: mantLow, high: ((sign << 31) | ((exp + 1023) << 20) | mantHigh) >>> 0 }
}

exports.writeSplitVarint64 = lowBits => highBits => {
  var buffer_ = []
  // Break the binary representation into chunks of 7 bits, set the 8th bit
  // in each chunk if it's not the final chunk, and append to the result.
  while (highBits > 0 || lowBits > 127) {
    buffer_.push((lowBits & 0x7f) | 0x80)
    lowBits = ((lowBits >>> 7) | (highBits << 25)) >>> 0
    highBits = highBits >>> 7
  }
  buffer_.push(lowBits)
  return buffer_
}

exports.splitInt64 = value => {
  // Convert to sign-magnitude representation.
  var sign = (value < 0)
  value = Math.abs(value)

  // Extract low 32 bits and high 32 bits as unsigned integers.
  var lowBits = value >>> 0
  var highBits = Math.floor((value - lowBits) /
                            TWO_TO_32)
  highBits = highBits >>> 0

  // Perform two's complement conversion if the sign bit was set.
  if (sign) {
    highBits = ~highBits >>> 0
    lowBits = ~lowBits >>> 0
    lowBits += 1
    if (lowBits > 0xFFFFFFFF) {
      lowBits = 0
      highBits++
      if (highBits > 0xFFFFFFFF) highBits = 0
    }
  }

  return { low: lowBits, high: highBits }
}
