package com.gu.openplatform.contentapi.parser

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import JsonFileLoader._

class TagsJsonParserTest extends FlatSpec with ShouldMatchers {

  // generated by:
  // http://content.guardianapis.com/tags.json
  //   ?page-size=2&format=json
  lazy val tagsResponse = JsonParser.parseTags(loadFile("tags.json"))

  "tags endpoint parser" should "parse basic reponse header" in {
    tagsResponse.status should be ("ok")
    tagsResponse.userTier should be ("free")
  }

  it should "parse pagination" in {
    tagsResponse.startIndex should be (1)
    tagsResponse.pageSize should be (2)
    tagsResponse.currentPage should be (1)
    tagsResponse.pages should be (14619)
    tagsResponse.total should be (29238)
  }

  it should "parse the tags" in {
    tagsResponse.results.size should be (2)
    tagsResponse.results.head.webTitle should be ("Mohamed Abdul Malek")
    tagsResponse.results.head.bio should be (Some("<p>Mohamed Abdul Malek is chairman of Libya Watch</p>"))
  }
}