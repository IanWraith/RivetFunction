package com.e2k.RivetFunction

import scala.collection.mutable.ArrayBuffer 
import scala.collection.mutable.StringBuilder

class XPA {
  
  def decode (waveData : WaveData) : List[String]=	{
    var displayArray:ArrayBuffer[String]=new ArrayBuffer() 
    // Set the samples per symbol
    // XPA is 10 baud
    val samplesPerBaud=samplesPerSymbol(10,waveData.sampleRate)
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
    var dline=new StringBuilder("")
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
      
      var it=0
      var pos=0
      var lastChar=""
      while (pos<=(dataEnd-samplesPerBaud))	{
        pos=sync+(it*samplesPerBaud)
        
        // Debug code
        if (pos==37005)	{
	        var xline=new StringBuilder("")
            var x=pos
            while (x<(37005+samplesPerBaud))	{
              xline.append(waveData.rawList(x))
              xline.append(",")
              x=x+1
            }
	        displayArray+=xline.toString
        	}
        
        val nxt=measureSegmentFrequency(waveData,pos,samplesPerBaud)
        val char=getChar(nxt,lastChar)
        dline.append(char)
        lastChar=char
        tline.clear
        tline.append("Received Tone "+nxt+"Hz maps to "+char+" at position "+pos)
        displayArray+=tline.toString()
        it=it+1
      }
      
   
    }
    else displayArray+="No sync found !"
    
    displayArray+=dline.toString()
  
    (displayArray.toList)
  }
  
  // Measure the frequency of a segment returning the frequency in Hertz and a quality indicator
  def measureSegmentFrequency (waveData : WaveData , startPoint : Int ,len : Int): Int=	{
	var peakArray:ArrayBuffer[Int]=new ArrayBuffer() 
    var lastPeak=0
    // was +1
    var b=startPoint+1
    // was -2
    val endPoint=(startPoint+len)-2    
    // Find the distances between the peaks in the selected segment    
    while (b<endPoint)	{
      // TODO : Stop this line from going out of bounds
      if (peakDetect(waveData.rawList(b-1),waveData.rawList(b),waveData.rawList(b+1),waveData.rawList(b+2))==true)	{
      	if (lastPeak>0) peakArray+=(b-lastPeak)
        lastPeak=b
      }
       b=b+1
     }
   (measureFrequencyMean(peakArray.toList,waveData.sampleRate,waveData.correctionFactor)) 
  }

  // Detect two different kinds of waveform peaks
  def peakDetect (p0:Int,p1:Int,p2:Int,p3:Int) : Boolean=	{
    if ((p1>p0)&&(p1>p2)) return (true)
    else if ((p1>p0)&&(p1==p2)&&(p1>p3)) return (true)
    return (false)
  }
  
  // Finds the mean value in the peakArray List
  def measureFrequencyMean (peakList:List[Int],sampleFreq : Double,correctionFactor:Int): Int =	{
    var total=0.0
    for (peak <- peakList)	{
        total+=peak   
    }    
    (getFrequency((total/peakList.length),sampleFreq,correctionFactor))
  }  
  
  
  // Convert the mean distance between peaks into a frequency measurement
  def getFrequency (dmean : Double,sampleFreq : Double,correctionFactor : Int) : Int={
    ((1.0/((1.0/sampleFreq)*dmean)).toInt)-correctionFactor
  }
  
  // Return the number of samples per baud
  def samplesPerSymbol (dbaud : Double,sampleFreq : Double) : Int={
    (sampleFreq/dbaud).toInt
  }
  
  // Hunt for a high or a low start tone
  def startHunt (waveData : WaveData,end :Int,samplesPerBaud :Int) : Tuple3[Int,Int,String] ={
    var start=0
    // Allow up to 100 Hz
    val errorAllowance=100
    while(start<end)	{
      val ret=measureSegmentFrequency(waveData,start,(samplesPerBaud*1))
      val low=toneTest(ret,520,errorAllowance)
      if (low._1==true) return (start,(low._2),"Low Start Tone Found")
      // High
      val high=toneTest(ret,1280,errorAllowance)
      if (high._1==true) return (start,(high._2),"High Start Tone Found")
      start=start+1
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
    else if ((tone>(680-lw))&&(tone<(680+lw))) return ("Group Space")
    else if ((tone>(720-lw))&&(tone<(720+lw))) return ("End Tone")
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
    else if ((tone>(1160-lw))&&(tone<(1160+lw))) return ("Message Start")
    else if ((tone>(1200-lw))&&(tone<(1200+lw))) return ("Repeat")
    else if ((tone>(1280-lw))&&(tone<(1280+lw))) return ("Start High")
    else return ("UNID")
  }
  
  

}