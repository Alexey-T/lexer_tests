package com.mg.twitbay.dsl

import groovyx.twitter.Twitter as Twitter

/**
 * TwitBayDSL.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class TwitBayDSL{
	
	def info = [:]

	
	def productId
	def eBayAppId
	def twUser
	def twPassword	
	
	
	def methodMissing(String name, args) { 
		info[name] = args[0]
	} 
	

	//@TODO
	//trocar o nome desse metodo.......... para publish e 
	//colocar como parametro do contrutor da classe o closure desde metodo...
	//
	def getProduct(closure) {
		closure.delegate = this 
		closure() 
		if (checkParameters().toString() == 'true') {
			
			//get content from eBay...
			def last = lastBid()
			
			//post updates on twitter....
			post last
			
		} else {
			println checkParameters()
		}   
	}
	
	
	def lastBid() {
		
		def baseUrl = '''http://open.api.ebay.com/shopping?callname=GetSingleItem&responseencoding=XML&appid=$eBayAppId&siteid=0&version=603&ItemID=$productId'''
		def engine   = new groovy.text.SimpleTemplateEngine()       
		def template = engine.createTemplate(baseUrl)   	    
		
		def url = template.make(info).toString()
		def ns = new groovy.xml.Namespace("urn:ebay:apis:eBLBaseComponents", 'ns')      
		def root = new XmlParser().parse(url) 
		
		//data from webservice....
		def itemID = root[ns.Item][ns.ItemID].text()
		def timeStamp = root[ns.Timestamp].text()
		def bidCount =  root[ns.Item][ns.BidCount].text()      
		def currency = root[ns.Item][ns.ConvertedCurrentPrice].'@currencyID'.text()
		def price = root[ns.Item][ns.ConvertedCurrentPrice].text()
		def title = root[ns.Item][ns.Title].text()
		def status = root[ns.Item][ns.ListingStatus].text()
		
		
		if (sameAsBefore([itemID:itemID, bids:bidCount, price:price, status:status])) {
			return null
		}
		
		def line = "Product: ${title} @ ${timeStamp} / Bids:${bidCount} / Price:${currency} ${price} - Status:${status}" 
	}
	
	def post(last) {
		println "post........... ${last}"
		if (last) {
			def twitter = new Twitter(info.twUser, info.twPassword)
			twitter.postUpdate(last)
			twitter.logoff()
		}
	}
	
	def sameAsBefore(Map map) {
		println map.dump()
		false
	}
	
	
	def checkParameters() {
		if (info.containsKey("productId") && info.containsKey("eBayAppId") && info.containsKey("twUser") && info.containsKey("twPassword")) {
			return true
		} else {
			return "Please inform the right parameters: productId, eBayAppId, twUser and twPassword!"
		}
	}
	
	
}
