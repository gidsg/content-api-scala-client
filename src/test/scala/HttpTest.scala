import scala.concurrent.ExecutionContext
import com.gu.openplatform.contentapi.model.{ItemResponse, Content}
import com.gu.openplatform.contentapi.{DispatchAsyncApi, ApiError, SyncApi}
import com.gu.openplatform.contentapi.connection._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FlatSpec, BeforeAndAfterEach}
import dispatch._


class HttpTest extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  import ExecutionContext.Implicits.global

  info("Tests that each type of client can actually perform a call to the api")

  val knownItemId = "science/political-science/2013/jul/29/messing-nature-geoengineering-green-thought"

  "ApacheSyncHttpClient" should "be able to call the api" in {
    testClient(new SyncApi with ApacheSyncHttpClient)
  }

  "MultiThreadedApacheSyncHttpClient" should "be able to call the api" in {
    testClient(new SyncApi with MultiThreadedApacheSyncHttpClient)
  }

  "JavaNetSyncHttp" should "be able to call the api" in {
    testClient(new SyncApi with JavaNetSyncHttp)
  }

  val syncApi = new SyncApi with DispatchSyncHttp {
    implicit val executionContext = ExecutionContext.global
  }

  "DispatchSyncHttp" should "be able to call the api" in {
    testClient(syncApi)
  }

  "DispatchSyncHttp" should "follow redirects" in {
    //redirects to /video
    syncApi.item.itemId("type/video").response.tag.get.id should be("type/video")
  }

  "DispatchAsyncHttp" should "be able to call the api" in {

    val promisedContent: Future[Content] = for {
      response <- DispatchAsyncApi.item.itemId(knownItemId).response
    } yield response.content.get

    promisedContent.apply().id should be (knownItemId)
  }

  "DispatchAsyncHttp" should "return API errors as a broken promise" in {
    val brokenPromise: Future[ItemResponse] = DispatchAsyncApi.item.itemId("fdsfgs").response
    brokenPromise.recover { case error => error should be (ApiError(404, "Not Found")) } apply()
  }

  override protected def beforeEach() {
    // avoid upsetting rate limit of free tier,
    Thread.sleep(500)
  }

  private def testClient(api: SyncApi) {
    val content = api.item.itemId(knownItemId).response
      .content.get

    content.id should be (knownItemId)
  }

}
