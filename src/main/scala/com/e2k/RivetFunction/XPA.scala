package com.e2k.RivetFunction

import scala.collection.mutable.ArrayBuffer 
import scala.collection.mutable.StringBuilder

class XPA {
  
  def decode (waveData : WaveData) : List[String]=	{
    var displayArray:ArrayBuffer[String]=new ArrayBuffer() 
    // Set the samples per symbol
    // XPA is 20 baud
    val samplesPerBaud=samplesPerSymbol(20,waveData.sampleRate)
    val dataEnd=waveData.rawList.length-samplesPerBaud
    displayArray+="XPA Decode"
    // Hunt for a start tone
    val start=startHunt(waveData,dataEnd,samplesPerBaud)
    // No start tone found
    if (start._1== -1)	{
      displayArray+=start._3
      return (displayArray.toList)
    }
    displayArray+=start._3
    // Set the correction factor
    waveData.correctionFactor=start._2

    var tline=new StringBuilder("")
    tline.append("Error correction factor is ")
    tline.append(start._2)
    tline.append( "Hz")
    displayArray+=tline.toString()
    
    val sync=alternatingSyncHunt(waveData,start._1,dataEnd-samplesPerBaud,samplesPerBaud)
    
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
   (measureFrequencyMode(peakArray,waveData.sampleRate)) 
  }
  
  // Finds the mode value in the peakArray
  def measureFrequencyMode (peakArray:ArrayBuffer[Int],sampleFreq : Double): Tuple2 [Int,Int]=	{
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
    ((getFrequency(modal,sampleFreq)),((mfreq/peakArray.length)*100.0).toInt)
  }
  
  // Convert the modal distance between peaks into a frequency measurement
  def getFrequency (dmodal : Double,sampleFreq : Double) : Int={
    ((1.0/((1.0/sampleFreq)*dmodal)).toInt)
  }
  
  // Return the number of samples per baud
  def samplesPerSymbol (dbaud : Double,sampleFreq : Double) : Int={
    (sampleFreq/dbaud).toInt
  }
  
  // Hunt for a high or a low start tone
  def startHunt (waveData : WaveData,end :Int,samplesPerBaud :Int) : Tuple3[Int,Int,String] ={
    var start=100
    // Allow up to 100 Hz
    val errorAllowance=100
    val goodSymbol=40
    while(start<end)	{
      val ret=measureSegmentFrequency(waveData,start,samplesPerBaud)
      if (ret._2>goodSymbol)	{
        // Low
        val low=toneTest(ret._1,520,errorAllowance)
        if (low._1==true) return (start,(low._2),"Low Start Tone Found")
        // High
        val high=toneTest(ret._1,1280,errorAllowance)
        if (high._1==true) return (start,(high._2),"High Start Tone Found")
      }
      start=start+10
    }
    // We have a problem
    (-1,-1,"Error ! No Start tone found.")
  }
  
  // Test for a specific tone
  def toneTest (freq : Int,tone : Int,errorAllow : Int) : Tuple2[Boolean,Int] =	{
    if ((freq>(tone-errorAllow))&&(freq<(tone+errorAllow))) return (true,(freq-tone))
     else return (false,0)
  }
  
  def alternatingSyncHunt (waveData:WaveData,start:Int,end:Int,samplesPerBaud :Int) : Int ={
    var a=start
    // Look for a sync low (600 Hz) followed by a sync high (1120 Hz)
    while (a<end)	{
        val low=seekSyncLow(waveData,a,samplesPerBaud)
        if (low!= -1)	{
            println ("Low sync found")
        	val high=seekSyncHigh(waveData,a+samplesPerBaud,samplesPerBaud)
        	if (high!= -1) return (a)
        }
    	a=a+100
    }
    (-1)
  }
  
  def seekSyncLow (waveData:WaveData,start:Int,samplesPerBaud :Int) : Int = 	{
    val thigh=measureSegmentFrequency(waveData,start,start+samplesPerBaud)
    if (thigh._2>40)	{
    	val htone=toneTest(thigh._1,600,100)
        if (htone._1==true) return (htone._2)
        }
    (-1)
  }
  
  def seekSyncHigh (waveData:WaveData,start:Int,samplesPerBaud :Int) : Int = 	{
    val thigh=measureSegmentFrequency(waveData,start,start+samplesPerBaud)
    if (thigh._2>40)	{
    	val htone=toneTest(thigh._1,1120,100)
        if (htone._1==true) return (htone._2)
        }
    (-1)
  }
  
  

}