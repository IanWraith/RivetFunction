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
  
  // Measure the frequency of a segment returning the frequency in Hertz and a quality indicator
  def measureSegmentFrequency (waveData : WaveData , startPoint : Int ,len : Int): Tuple2 [Int,Int]=	{
	var peakArray:ArrayBuffer[Int]=new ArrayBuffer() 
    var lastPeak=0
    var b=startPoint+1
    // Find the distances between the peaks in the selected segment    
    while (b<(startPoint+len))	{
      if ((waveData.rawList(b)>waveData.rawList(b-1))&&(waveData.rawList(b)>waveData.rawList(b+1)))	{
        peakArray+=(b-lastPeak)
        lastPeak=b
      }
       b=b+1
    }
   (measureFrequency(peakArray,waveData.sampleRate)) 
  }
  
  // Finds the mode value in the peakArray
  def measureFrequency (peakArray:ArrayBuffer[Int],sampleFreq : Double): Tuple2 [Int,Int]=	{
    // Find the mode value of the peakArray ArrayBuffer
    var modal = 0;
    var mfreq = 0;
    var i=0;
    var j=0;
    while(i<peakArray.length) {
        var freq = 0;
        while(j<peakArray.length) {
            if (j == i ) freq=freq+1
            j=j+1
        }
        if( freq > mfreq ) {
            modal = i;
            mfreq = freq;
    }
    i=i+1
    }
    val freq=getFrequency(modal:Int,sampleFreq:Double)
    (freq,mfreq)
  }
  
  

}