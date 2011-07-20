package com.e2k.RivetFunction

import scala.collection.mutable.ArrayBuffer 

class XPA2 extends MFSK {
  
  def decode (waveData:WaveData,baudRate:Int) : List[String]=	{
    var displayArray:ArrayBuffer[String]=new ArrayBuffer() 
    // Set the samples per symbol
    // XPA2
    val samplesPerBaud=samplesPerSymbol(baudRate,waveData.sampleRate)
    val dataEnd=waveData.rawList.length-samplesPerBaud
    displayArray+="XPA2 Decode\n"
    // Hunt for a start tone
    println("Hunting for a start tone")
    val start=startHunt(waveData,dataEnd.toInt,samplesPerBaud.toInt)
    // No start tone found
    if (start._1== -1)	{
      displayArray+=start._3
      return (displayArray.toList)
    }
    displayArray+=start._3
    println("Start tone found")
    // Double check the start tone with a DCT
    val cstart=doDCT(waveData,start._1,samplesPerBaud.toInt,samplesPerBaud)
    // Set the correction factor from the DCT frequency which is more accurate
    waveData.correctionFactor=cstart-965
    // Let the user know the correction factor
    var tline=new StringBuilder("")
    tline.append("Error correction factor is ")
    tline.append(waveData.correctionFactor)
    tline.append( "Hz")
    tline.append(" found at position ")
    tline.append(start._1+"\n")
    displayArray+=tline.toString()
    println("Trying to acquire syncronisation")
    // Hunt for the alternating sync tone    
    val sync=alternatingSyncHunt(waveData,start._1,(dataEnd.toInt-(samplesPerBaud.toInt*2)),samplesPerBaud.toInt)
    tline.clear
    if (sync > 0 )	{
      tline.append("Sync found at position "+sync.toInt+"\n");
      displayArray+=tline.toString()
      println("Syncronisation Acquired")
      //displayArray+=getMessage(waveData,sync,dataEnd,samplesPerBaud)
    }
    else displayArray+="No sync found !\n"
    (displayArray.toList)
  }
  
  // Hunt for a start tone
  def startHunt (waveData : WaveData,end :Int,samplesPerBaud :Int) : Tuple3[Int,Int,String] ={
    var start=0
    // Allow up to 50 Hz error
    val errorAllowance=50
    while(start<end)	{
      val ret=measureSegmentFrequency(waveData,start,(samplesPerBaud*1))
      val low=toneTest(ret,965,errorAllowance)
      if (low._1==true) return (start,(low._2),"Start Tone Found\n")
      start=start+1
    }
    // We have a problem
    (-1,-1,"Error ! No Start tone found.\n")
  }
  
  // Look for a sync low (997 Hz) followed by a sync high (1037 Hz)
  def alternatingSyncHunt (waveData:WaveData,start:Int,end:Int,samplesPerBaud :Int) : Int ={
    var a=start
    while (a<end)	{
        val low=seekSyncLow(waveData,a,samplesPerBaud)
        if (low!= -1)	{
        	val high=seekSyncHigh(waveData,(a+samplesPerBaud),samplesPerBaud)
        	if (high!= -1)	{
        	  val dcheck=doDCT(waveData,(a+samplesPerBaud),samplesPerBaud,samplesPerBaud)
        	  val dtone=toneTest(dcheck,1037,10)
        	  if (dtone._1==true) return (a)
        	}
        }
    	a=a+1
     }
    (-1)
  }
  
  // Check a slot for the 997 Hz low sync tone
  def seekSyncLow (waveData:WaveData,start:Int,samplesPerBaud :Int) : Int = 	{
    val thigh=measureSegmentFrequency(waveData,start,samplesPerBaud)
    val htone=toneTest(thigh,997,10)
    if (htone._1==true)	{
      return (htone._2) 
    }
    else (-1)
  }
  
  // Check a slot for the 1037 Hz high sync tone
  def seekSyncHigh (waveData:WaveData,start:Int,samplesPerBaud :Int) : Int = 	{
    val thigh=measureSegmentFrequency(waveData,start,samplesPerBaud)
    val htone=toneTest(thigh,1037,10)
    if (htone._1==true)	{
      return (htone._2)  
    }
    else (-1)
  }
  

}