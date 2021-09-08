<script>
  export let name;
</script>

<style lang="scss">
  $color: red;

  h1 {
    color: $color;
  }

  div {
    background: green;

    > p {
      color: #fff;
    }
  }
</style>                                               

<style lang="sass">
  h1:
    color: $color;
  h2:
    color: $color;
</style>                                               

<style lang='less'>
  isstring(lighten(1,2,3))
</style>


<h1>Hello {name}!</h1>

<div>
  <p>SASS is working!</p>
</div>

