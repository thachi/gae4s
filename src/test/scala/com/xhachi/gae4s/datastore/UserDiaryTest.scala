package com.xhachi.gae4s.datastore

import com.google.appengine.api.datastore.KeyFactory
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite

class UserDiaryTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  implicit val context = NoAncestorEntityStoreContext

  test("ancestorが指定されていないUserをcreateしようとするとエラーになること") {

    val key = KeyFactory.createKey(classOf[UserDiary].getName, 1)

    intercept[AssertionError] {
      val user1 = new UserDiary(new Key[UserDiary](key))
      user1.create()
    }

  }

  test("ダイアリーを２人分登録して別々に追加されること") {

    val user1 = new User(UserStore.allocateKey)
    UserStore.create(user1)
    val user2 = new User(UserStore.allocateKey)
    UserStore.create(user2)


    {
      val diaryKey = UserDiary.allocateKey(user1.key)
      assert(diaryKey.parent.isDefined)
      assert(diaryKey.parent.get == user1.key)

      val d = new UserDiary(diaryKey)
      d.create()

      val d2 = UserDiary.get(d.key)
      assert(d2.key.parent.isDefined)
      assert(d2.key.parent.get == user1.key)
    }

    {
      val diaryKey = UserDiary.allocateKey(user1.key)
      assert(diaryKey.parent.isDefined)
      assert(diaryKey.parent.get == user1.key)

      val d = new UserDiary(diaryKey)
      d.create()

      val d2 = UserDiary.get(d.key)
      assert(d2.key.parent.isDefined)
      assert(d2.key.parent.get == user1.key)
    }

    {
      val diaryKey = UserDiary.allocateKey(user2.key)
      assert(diaryKey.parent.isDefined)
      assert(diaryKey.parent.get == user2.key)

      val d = new UserDiary(diaryKey)
      d.create()

      val d2 = UserDiary.get(d.key)
      assert(d2.key.parent.isDefined)
      assert(d2.key.parent.get == user2.key)
    }

    assert(UserDiary.query(user1.key).count == 2)
    assert(UserDiary.query(user2.key).count == 1)
    assert(UserDiary.query.count == 3)
  }
}


final class UserDiary(
                       val key: Key[UserDiary]
                       ) extends Entity[UserDiary] with Ancestor[User] {

}

class UserDiaryMeta extends EntityMeta[UserDiary] {

  def createEntity(key: Key[UserDiary]): UserDiary = new UserDiary(key)

  override def kind: String = "com.example.UserDiary"

  //  addApplyFromLLEntity {
  ////    (from: LLEntity, to) => from.tilke
  //  }
}


class UserDiaryStore
  extends EntityStore[UserDiary]
  with CreatableStore
  with UpdatableStore
  with QueryableStore
  with AllocatableKeyStore {

  override type ENTITY = UserDiary
  override type META = UserDiaryMeta

  override protected implicit val meta = new UserDiaryMeta
}


object UserDiary extends UserDiaryStore