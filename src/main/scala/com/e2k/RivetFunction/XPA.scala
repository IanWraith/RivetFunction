package com.e2k.RivetFunction

import scala.collection.mutable.ArrayBuffer 
import scala.collection.mutable.StringBuilder

class XPA extends MFSK {
    
  def decode (waveData:WaveData,baudRate:Int) : List[String]=	{
    var displayArray:ArrayBuffer[String]=new ArrayBuffer() 
    // Set the samples per symbol
    // XPA
    val samplesPerBaud=samplesPerSymbol(baudRate,waveData.sampleRate)
    val dataEnd=waveData.rawList.length-samplesPerBaud
    displayArray+="XPA Decode\n"
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
    // Set the correction factor
    waveData.correctionFactor=start._2
    var tline=new StringBuilder("")
    tline.append("Error correction factor is ")
    tline.append(start._2)
    tline.append( "Hz")
    tline.append(" found at position ")
    tline.append(start._1+"\n")
    displayArray+=tline.toString()
    println("Trying to acquire syncronisation")
    val sync=alternatingSyncHunt(waveData,start._1,(dataEnd.toInt-(samplesPerBaud.toInt*2)),samplesPerBaud.toInt)
    tline.clear
    if (sync > 0 )	{
      tline.append("Sync found at position "+sync.toInt+"\n");
      displayArray+=tline.toString()
      println("Syncronisation Acquired")
      displayArray+=getMessage(waveData,sync,dataEnd,samplesPerBaud)
    }
    else displayArray+="No sync found !\n"
    (displayArray.toList)
  }
  
  // Hunt for a high or a low start tone
  def startHunt (waveData : WaveData,end :Int,samplesPerBaud :Int) : Tuple3[Int,Int,String] ={
    var start=0
    // Allow up to 50 Hz
    val errorAllowance=50
    while(start<end)	{
      val ret=measureSegmentFrequency(waveData,start,(samplesPerBaud*1))
      val low=toneTest(ret,520,errorAllowance)
      if (low._1==true) return (start,(low._2),"Low Start Tone Found\n")
      // High
      val high=toneTest(ret,1280,errorAllowance)
      if (high._1==true) return (start,(high._2),"High Start Tone Found\n")
      start=start+1
    }
    // We have a problem
    (-1,-1,"Error ! No Start tone found.\n")
  }
  
  // Test for a specific tone
  def toneTest (freq : Int,tone : Int,errorAllow : Int) : Tuple2[Boolean,Int] =	{
    if ((freq>(tone-errorAllow))&&(freq<(tone+errorAllow))) return (true,(freq-tone))
     else return (false,0)
  }
  
  // Look for a sync low (600 Hz) followed by a sync high (1120 Hz)
  def alternatingSyncHunt (waveData:WaveData,start:Int,end:Int,samplesPerBaud :Int) : Int ={
    var a=start
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
  
  // Check a slot for the 600 Hz low sync tone
  def seekSyncLow (waveData:WaveData,start:Int,samplesPerBaud :Int) : Int = 	{
    val thigh=measureSegmentFrequency(waveData,start,samplesPerBaud)
    val htone=toneTest(thigh,600,15)
    if (htone._1==true)	{
      return (htone._2) 
    }
    else (-1)
  }
  
  // Check a slot for the 1120 Hz high sync tone
  def seekSyncHigh (waveData:WaveData,start:Int,samplesPerBaud :Int) : Int = 	{
    val thigh=measureSegmentFrequency(waveData,start,samplesPerBaud)
    val htone=toneTest(thigh,1120,15)
    if (htone._1==true)	{
      return (htone._2)  
    }
    else (-1)
  }
  
  // Return a String for a tone
  def getChar (tone : Int,prevChar : String) : String =	{
    val lw=21
    if ((tone>(520-lw))&&(tone<(520+lw))) return ("Start Low")
    else if ((tone>(600-lw))&&(tone<(600+lw))) return ("Sync Low")
    else if ((tone>(680-lw))&&(tone<(680+lw))) return (" ")
    else if ((tone>(720-lw))&&(tone<(720+lw))) return ("\nEnd Tone")
    else if ((tone>(760-lw))&&(tone<(760+lw))) return ("0")
    else if ((tone>(800-lw))&&(tone<(800+lw))) return ("1")
    else if ((tone>(840-lw))&&(tone<(840+lw))) return ("2")
    else if ((tone>(880-lw))&&(tone<(880+lw))) return ("3")
    else if ((tone>(920-lw))&&(tone<(920+lw))) return ("4")
    else if ((tone>(960-lw))&&(tone<(960+lw))) return ("5")
    else if ((tone>(1000-lw))&&(tone<(1000+lw))) return ("6")
    else if ((tone>(1040-lw))&&(tone<(1040+lw))) return ("7")
    else if ((tone>(1080-lw))&&(tone<(1080+lw))) return ("8")
    else if ((tone>(1120-lw))&&(tone<(1120+lw)))	{
      if (prevChar=="Sync Low") return ("Sync High")
      else return ("9")
    }
    else if ((tone>(1160-lw))&&(tone<(1160+lw))) return ("\nMessage Start")
    else if ((tone>(1200-lw))&&(tone<(1200+lw))) return ("R")
    else if ((tone>(1280-lw))&&(tone<(1280+lw))) return ("Start High")
    else return ("UNID")
  }
  
  // Decode the XPA message
  def getMessage (waveData:WaveData,sync:Double,dataEnd:Double,samplesPerBaud:Double) : String =	{
    var tline=new StringBuilder("")
    var it=0
    var pos=0.0
    var lastChar=""
    var groupCount=0
    while (pos<=(dataEnd-samplesPerBaud))	{
        pos=sync+(it*samplesPerBaud)        
        val nxt=doDCT(waveData,pos.toInt,samplesPerBaud.toInt,samplesPerBaud)
        val char=getChar(nxt,lastChar)
        // If a Repeat is signalled display the last character
        // If not display this character
        if (char=="R")  tline.append(lastChar)
        else if ((char !="Sync High")&&(char !="Sync Low")) tline.append(char)
        // If we get two End Tones in a row then stop decoding
        if ((char=="R")&&(lastChar=="\nEnd Tone")) return (tline.toString)
        // Hunt for 6666622662626
        if (tline.lastIndexOf("6666622662626")==(tline.length-13))	{
          tline.append("\n")
          groupCount=0
        }
        // Hunt for 4444444444
        if (tline.lastIndexOf("4444444444")==(tline.length-10))	{
          tline.append("\n")
          groupCount=0
        }
        // Count the group spaces
        if (char==" ") groupCount=groupCount+1
        // After 10 group spaces add a line break
        if (groupCount==10)	{
          groupCount=0
          tline.append("\n")
        }
        // Record the last character
        lastChar=char
        // Display Unknowns
        if (char=="UNID") tline.append(" ("+nxt+" Hz) at position "+pos.toInt+"\n");
        it=it+1
      }
    tline.toString
  }
  
}