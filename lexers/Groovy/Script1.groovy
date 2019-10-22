list = this.class.methods.name.grep(~/get.*/).sort()
list.collect { println it }