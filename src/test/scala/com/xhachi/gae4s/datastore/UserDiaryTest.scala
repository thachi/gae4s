package com.xhachi.gae4s.datastore

import org.scalatest.FunSuite
import com.xhachi.gae4s.tests.AppEngineTestSuite
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig

class UserDiaryTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: super.getConfig

  test("ダイアリー") {

    val user1 = new User(UserStore.allocateKey)
    UserStore.create(user1)
    val user2 = new User(UserStore.allocateKey)
    UserStore.create(user2)


    val user1DiaryStore = UserDiaryStore(user1.key)
    val user2DiaryStore = UserDiaryStore(user2.key)

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
                       ) extends Entity[UserDiary] {

}

class UserDiaryMeta extends EntityMeta[UserDiary] {

  override protected def createEntity(key: Key[UserDiary]): UserDiary = new UserDiary(key)

  override def kind: String = "com.example.UserDiary"

  //  addApplyFromLLEntity {
  ////    (from: LLEntity, to) => from.tilke
  //  }
}


class UserDiaryStore private(val parentKey: Key[User])
  extends DescendantEntityStore[UserDiary, User]
  with QueryableStore
  with UpdatableStore
  with AutoAllocateKeyStore {

  override type ENTITY = UserDiary
  override type META = UserDiaryMeta

  override protected def datastore = Datastore

  override protected implicit val meta = new UserDiaryMeta
}


object UserDiaryStore extends UserDiaryStore(null) {
  def apply(parentKey: Key[User]) = new UserDiaryStore(parentKey)
}