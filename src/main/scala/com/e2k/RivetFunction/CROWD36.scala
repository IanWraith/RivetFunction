package com.e2k.RivetFunction

import scala.collection.mutable.ArrayBuffer 

class CROWD36 extends MFSK {
  
  def decode (waveData:WaveData,baudRate:Int) : List[String]=	{
    var displayArray:ArrayBuffer[String]=new ArrayBuffer() 
    // Set the samples per symbol
    // XPA2
    val samplesPerBaud=samplesPerSymbol(baudRate,waveData.sampleRate)
    val dataEnd=waveData.rawList.length-samplesPerBaud
    displayArray+="CROWD36 Decode\n"
      
      
    (displayArray.toList)
  }
  

}