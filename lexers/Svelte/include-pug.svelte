<template lang="pug">
  div Posts +each('posts as post') a(href="{post.url}") {post.title}
</template>
