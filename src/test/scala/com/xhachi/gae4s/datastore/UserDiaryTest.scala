package com.xhachi.gae4s.datastore

import org.scalatest.FunSuite
import com.xhachi.gae4s.tests.AppEngineTestSuite
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig

class UserDiaryTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("ダイアリー") {

    val user1 = new User(User.allocateKey)
    User.create(user1)
    val user2 = new User(User.allocateKey)
    User.create(user2)


    val user1DiaryStore = new UserDiaryStore(user1.key)
    val user2DiaryStore = new UserDiaryStore(user2.key)

    {
      val diaryKey = user1DiaryStore.allocateKey
      assert(diaryKey.parent.isDefined)
      assert(diaryKey.parent.get == user1.key)

      val d = new UserDiary(diaryKey)
      user1DiaryStore.create(d)

      val d2 = user1DiaryStore.get(d.key)
      assert(d2.key.parent.isDefined)
      assert(d2.key.parent.get == user1.key)
    }

    {
      val diaryKey = user1DiaryStore.allocateKey
      assert(diaryKey.parent.isDefined)
      assert(diaryKey.parent.get == user1.key)

      val d = new UserDiary(diaryKey)
      user1DiaryStore.create(d)

      val d2 = user1DiaryStore.get(d.key)
      assert(d2.key.parent.isDefined)
      assert(d2.key.parent.get == user1.key)
    }

    {
      val diaryKey = user2DiaryStore.allocateKey
      assert(diaryKey.parent.isDefined)
      assert(diaryKey.parent.get == user2.key)

      val d = new UserDiary(diaryKey)
      user2DiaryStore.create(d)

      val d2 = user2DiaryStore.get(d.key)
      assert(d2.key.parent.isDefined)
      assert(d2.key.parent.get == user2.key)
    }

    assert(user1DiaryStore.query.count == 2)
    assert(user2DiaryStore.query.count == 1)

    assert(UserDiaryStore.query.count == 3)



  }
}


final class UserDiary(
                       val key: Key[UserDiary]

                       ) extends LeafEntity[UserDiary, User] {

}

class UserDiaryMeta extends EntityMeta[UserDiary] {

  override protected def createEntity(key: Key[UserDiary]): UserDiary = new UserDiary(key)

  override def kind: String = "com.example.UserDiary"

  //  addApplyFromLLEntity {
  ////    (from: LLEntity, to) => from.tilke
  //  }
}


class UserDiaryStore(val parentKey: Key[User]) extends LeafEntityStore[User] with QueryableStore with UpdatableStore with AutoAllocateKeyStore {
  override type M = UserDiaryMeta
  override type E = UserDiary

  override protected def datastore = Datastore

  override protected implicit val meta = new UserDiaryMeta
}


object UserDiaryStore extends UserDiaryStore(null) {

}




