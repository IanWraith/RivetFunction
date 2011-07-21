package com.e2k.RivetFunction

import scala.collection.mutable.ArrayBuffer 

class CROWD36 extends MFSK {
  
  def decode (waveData:WaveData,baudRate:Int) : List[String]=	{
    var displayArray:ArrayBuffer[String]=new ArrayBuffer() 
    // Set the samples per symbol
    val samplesPerBaud=samplesPerSymbol(baudRate,waveData.sampleRate)
    val dataEnd=waveData.rawList.length-samplesPerBaud
    displayArray+="CROWD36 Decode\n"
      
    val syncPoint=syncCROWD36(waveData,samplesPerBaud)
      
      
    (displayArray.toList)
  }
  
  def syncCROWD36 (waveData:WaveData,samplesPerBaud:Double) : Int=	{
    var point=0
    var highest= -1
    var highPoint= -1
    // Look for the highest energy point at any point in the first
    // 10% of the data
    val end10=(waveData.rawList.length/100)*10
    while(point<end10)	{
      val thigh=doDCTHighPower(waveData,point,samplesPerBaud.toInt,samplesPerBaud)
      if (thigh>highest)	{
        highest=thigh
        highPoint=point
        
        println("High found at "+highPoint)
        
      }
      point=point+1
    }
    return (highPoint)
  }
  

}