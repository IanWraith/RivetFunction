package com.e2k.RivetFunction

import scala.collection.mutable.ArrayBuffer 

class WaveData {
  var endian=false
  var sampleRate=0.0
  var channels=0
  var sampleSizeInBits=0
  var bytesPerFrame=0
  var rawList:ArrayBuffer[Int]=new ArrayBuffer() 
  var correctionFactor=0
}