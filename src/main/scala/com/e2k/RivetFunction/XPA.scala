package com.e2k.RivetFunction

import scala.collection.mutable.ArrayBuffer 
import scala.collection.mutable.StringBuilder

class XPA {
  
  def decode (waveData : WaveData) : List[String]=	{
    var displayArray:ArrayBuffer[String]=new ArrayBuffer() 
    
    measureSegmentFrequency(waveData,100,200)
    //var tline=new StringBuilder("")
    
    //var a=0
    //while (a<zcPoints.length)	{
      //tline.append(zcPoints(a))
      //tline.append(" ")
      //a=a+1
      //if (a%50==0) tline.append("\n")
    //}
    //displayArray+=tline.toString()
    
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
    var modal=0
    var mfreq=0.0
    var i=1
    while(i<peakArray.length) {
        var freq=0;
        var j=1
        while(j<peakArray.length) {
            if (peakArray(j)==peakArray(i)) freq=freq+1
            j=j+1
        }
        if(freq>mfreq ) {
            modal=peakArray(i)
            mfreq=freq
        	}       
    i=i+1
    }
    mfreq=(mfreq/peakArray.length)*100.0
    val afreq=getFrequency(modal,sampleFreq)
    (afreq,mfreq.toInt)
  }
  
  // Convert the modal distance between peaks into a frequency measurement
  def getFrequency (modal : Int , sampleFreq : Double) : Int={
    val dmodal : Double=modal
    val r=1.0/((1.0/sampleFreq)*dmodal)
    (r.toInt)
  }
  

}