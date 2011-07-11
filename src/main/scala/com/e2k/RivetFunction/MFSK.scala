package com.e2k.RivetFunction

import scala.collection.mutable.ArrayBuffer 

trait MFSK {
  
  // Measure the frequency of a segment returning the frequency in Hertz and a quality indicator
  def measureSegmentFrequency (waveData : WaveData , startPoint : Int ,len : Int): Int=	{
	var peakArray:ArrayBuffer[Int]=new ArrayBuffer() 
    var lastPeak=0
    var b=startPoint+1
    val endPoint=(startPoint+len)-2    
    // Find the distances between the peaks in the selected segment    
    while (b<endPoint)	{
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
    val total:Double=peakList.sum
    (getFrequency((total/peakList.length),sampleFreq,correctionFactor))
  }  
  
  // Convert the mean distance between peaks into a frequency measurement
  def getFrequency (dmean : Double,sampleFreq : Double,correctionFactor : Int) : Int={
    ((1.0/((1.0/sampleFreq)*dmean)).toInt)-correctionFactor
  }
  
  // Return the number of samples per baud
  def samplesPerSymbol (dbaud : Double,sampleFreq : Double) : Double={
    sampleFreq/dbaud
  }
  
  // Run a Discrete Cosine Transformation on a section of the waveData raw data and return a frequency in Hz
  def doDCT (waveData:WaveData,start:Int,length:Int,samplesPerBaud:Double) : Int=	{
    var bin=0
    var k=0
    var highval=0.0
    var highbin= -1
    var transformData=0.0
    // Do the DCT
    while (bin<length)	{
      k=0
      transformData=0.0
      while (k<length)	{
        transformData+=(waveData.rawList(k+start))*Math.cos(bin*Math.Pi*k/length);
        k=k+1
      }
      // Check if this is the highest bin value so far
      if (transformData>highval)	{
          highval=transformData
          highbin=bin
        }
      bin=bin+1
    }
    if (highbin== -1) return (-1)
     else (calcFreqFromBin(highbin,waveData.sampleRate,samplesPerBaud,waveData.correctionFactor))
  }
  
  // Calculate the frequency in hertz from the DCT bin with the most power in
  def calcFreqFromBin (bin:Int,sampleFreq:Double,samplesPerBaud:Double,correctionFactor:Int) : Int={
    ((((sampleFreq/samplesPerBaud)*bin)/2.0).toInt-correctionFactor)
  }

  // Test for a specific tone
  def toneTest (freq:Int,tone:Int,errorAllow:Int) : Tuple2[Boolean,Int] =	{
    if ((freq>(tone-errorAllow))&&(freq<(tone+errorAllow))) return (true,(freq-tone))
     else return (false,0)
  }

}