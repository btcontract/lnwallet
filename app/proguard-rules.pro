-dontoptimize
-dontobfuscate
-dontpreverify
-ignorewarnings

-dontwarn scala.**

-keep class scala.collection.SeqLike { public protected *; }

-keepattributes Signature,*Annotation*

-dontwarn javax.annotation.**
