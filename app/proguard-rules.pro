-dontoptimize
-dontobfuscate
-dontpreverify
-ignorewarnings
-dontwarn scala.**

-keep class scala.collection.SeqLike {
    public protected *;
}