package com.xhachi.gae4s.datastore.annotations

import scala.annotation.StaticAnnotation

//TODO: 本当はフィールドにアノテーションを付けたいけれどもtraitのフィールドのアノテーションをマクロでとれていないので現在はこの形にしています。
/**
 * Datastoreに保存するための
 *
 * @param version 楽観的排他制御に使用するバージョンプロパティの名称
 * @param creationDate 作成日時を自動で設定するプロパティの名称
 * @param modificationDate 更新日時を自動で設定するプロパティの名称
 */
class entity(val version: String = "", val creationDate: String = "", val modificationDate: String = "") extends StaticAnnotation

class json extends StaticAnnotation

class serialize extends StaticAnnotation

class indexed extends StaticAnnotation

class version extends StaticAnnotation

class creationDate extends StaticAnnotation

class modificationDate extends StaticAnnotation

