package controllers

import models.geo.PointData
import java.util.Locale

trait JSONUtils {
  
  Locale.setDefault( Locale.ENGLISH )
  
  def wrapInJSONLD(json:String):String = s"""{ "@graph": [ $json ] }"""
  
	def makeJSONTerm( key: String, value: Float): String = {
    s""" "$key": $value """
  }
  
  def makeJSONTerm( key: String, value: String): String = {
    s""" "$key": "$value" """
  }

  def makeJSONTerm( key: String, value: PointData): String = {
    s""" "$key": "${value.id}", "timestamp": ${value.timestamp} """
  }
}