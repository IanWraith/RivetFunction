package com.e2k.RivetFunction

import scala.collection.mutable.ArrayBuffer 
import scala.collection.mutable.StringBuilder

class XPA {
  
  def decode (waveData : WaveData) : List[String]=	{
    var displayArray:ArrayBuffer[String]=new ArrayBuffer() 
    var zcPoints:ArrayBuffer[Int]=new ArrayBuffer() 
    
    zcPoints=peakDetect(waveData)
    var tline=new StringBuilder("")
    
    var a=0
    while (a<zcPoints.length)	{
      tline.append(zcPoints(a))
      tline.append(" ")
      a=a+1
      if (a%50==0) tline.append("\n")
    }
    displayArray+=tline.toString()
    
    (displayArray.toList)
  }
  
  def peakDetect (waveData : WaveData) : ArrayBuffer[Int]=	{
    var peakArray:ArrayBuffer[Int]=new ArrayBuffer() 
    var lastPeak=0
    var b=1
    
    println(waveData.rawList.length)
    
    while (b<waveData.rawList.length-3)	{
      if ((waveData.rawList(b)>waveData.rawList(b-1))&&(waveData.rawList(b)>waveData.rawList(b+1)))	{
        peakArray+=(b-lastPeak)
        lastPeak=b
      }
       b=b+1
    }
    (peakArray)
  }

}