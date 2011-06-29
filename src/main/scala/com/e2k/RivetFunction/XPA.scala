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
    tline.append(" found at position ")
    tline.append(start._1)
    displayArray+=tline.toString()
    
    tline.clear
    tline.append("File ends at position "+dataEnd)
    displayArray+=tline.toString()
    
    val sync=alternatingSyncHunt(waveData,start._1,(dataEnd-samplesPerBaud-samplesPerBaud),samplesPerBaud)
    tline.clear
    if (sync > 0 )	{
      tline.append("Sync found at position "+ sync);
      displayArray+=tline.toString()
      val nxt1=measureSegmentFrequency(waveData,2894,samplesPerBaud)
      tline.clear
      tline.append("Next freq is "+nxt1+ "Hz")
      displayArray+=tline.toString()
      
      val nxt2=measureSegmentFrequency(waveData,3977,samplesPerBaud)
      tline.clear
      tline.append("Next freq is "+nxt2+ "Hz")
      displayArray+=tline.toString()
      
      
    }
    else displayArray+="No sync found !"
      
  
    (displayArray.toList)
  }
  
  // Measure the frequency of a segment returning the frequency in Hertz and a quality indicator
  def measureSegmentFrequency (waveData : WaveData , startPoint : Int ,len : Int): Int=	{
	var peakArray:ArrayBuffer[Int]=new ArrayBuffer() 
    var lastPeak=0
    var b=startPoint+1
    val endPoint=(startPoint+len)-1    
    // Find the distances between the peaks in the selected segment    
    while (b<endPoint)	{
      // TODO : Stop this line from going out of bounds
      if ((waveData.rawList(b)>waveData.rawList(b-1))&&(waveData.rawList(b)>waveData.rawList(b+1)))	{
        peakArray+=(b-lastPeak)
        lastPeak=b
      }
       b=b+1
     }
   (measureFrequencyMean(peakArray,waveData.sampleRate)) 
  }
    
  // Finds the mean value in the peakArray
  def measureFrequencyMean (peakArray:ArrayBuffer[Int],sampleFreq : Double): Int =	{
    // Find the mean value of the peakArray ArrayBuffer
    var total=0
    var i=1
    while(i<peakArray.length) {       
        total=total+peakArray(i)     
    	i=i+1
    }
    ((getFrequency((total/i),sampleFreq)))
  }  
  
  
  // Convert the modal distance between peaks into a frequency measurement
  def getFrequency (dmodal : Double,sampleFreq : Double) : Int={
    ((1.0/((1.0/sampleFreq)*dmodal)).toInt)
  }
  
  // Return the number of samples per baud
  def samplesPerSymbol (dbaud : Double,sampleFreq : Double) : Int={
    (sampleFreq/dbaud).toInt*2
  }
  
  // Hunt for a high or a low start tone
  def startHunt (waveData : WaveData,end :Int,samplesPerBaud :Int) : Tuple3[Int,Int,String] ={
    var start=0
    // Allow up to 100 Hz
    val errorAllowance=100
    val goodSymbol=40
    while(start<end)	{
      val ret=measureSegmentFrequency(waveData,start,(samplesPerBaud*2))

      val low=toneTest(ret,520,errorAllowance)
      if (low._1==true) return (start,(low._2),"Low Start Tone Found")
      // High
      val high=toneTest(ret,1280,errorAllowance)
      if (high._1==true) return (start,(high._2),"High Start Tone Found")
      
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
        	val high=seekSyncHigh(waveData,(a+samplesPerBaud),samplesPerBaud)
        	if (high!= -1) return (a)
        }
    	a=a+1
     }
    (-1)
  }
  
  def seekSyncLow (waveData:WaveData,start:Int,samplesPerBaud :Int) : Int = 	{
    val thigh=measureSegmentFrequency(waveData,start,samplesPerBaud)
    val htone=toneTest(thigh,600,25)
    if (htone._1==true) return (htone._2) 
    (-1)
  }
  
  def seekSyncHigh (waveData:WaveData,start:Int,samplesPerBaud :Int) : Int = 	{
    val thigh=measureSegmentFrequency(waveData,start,samplesPerBaud)
    val htone=toneTest(thigh,1120,25)
    if (htone._1==true) return (htone._2)  
    (-1)
  }
  
  

}