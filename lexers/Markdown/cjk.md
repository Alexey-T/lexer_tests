~58 should NOT be strikethrough (no closing ~)
<div without closing > should NOT highlight rest of document as HTML

**测试。aa这里** should NOT leak bold to this text
**中文，aa标点；测试：OK！** all CJK punctuation should work
**aa**
**test。** blabla

```c
ddd
ddd
ddd
ddd
~~~c
dd
~~~

```
   
~~~c
ddd
```c
ddd
ddd
```

~~~

code:

    dd
    dd
    dd
    dd

