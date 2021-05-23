<script>
	let x = 0;
	let y = 0;

	function yPlusAValue(value) {
		return value + y;
	}

	$: total = yPlusAValue(x);
</script>

Total: {total}
<button on:click={() => x++}>
	Increment X
</button>

<button on:click={() => y++}>
	Increment Y
</button>


<style>
	:global(body) {
		/* this will apply to <body> */
		margin: 0;
	}

	div :global(strong) {
		/* this will apply to all <strong> elements, in any
			 component, that are inside <div> elements belonging
			 to this component */
		color: goldenrod;
	}
</style>


<div>
	<Widget/>
</div>

<button disabled={!clickable}>...</button>
<input required={false} placeholder="This input field is not required">
<div title={null}>This div has no title attribute</div>
<!-- These are equivalent -->
<button disabled={disabled}>...</button>
<button {disabled}>...</button>
<Widget foo={bar} answer={42} text="hello"/>
<h1>Hello {name}!</h1>
<p>{a} + {b} = {a + b}.</p>

{#if expression}...{/if}
{#if expression}...{:else if expression}...{/if}
{#if expression}...{:else}...{/if}

{#if answer === 42}
	<p>what was the question?</p>
{/if}

{#if porridge.temperature > 100}
	<p>too hot!</p>
{:else if 80 > porridge.temperature}
	<p>too cold!</p>
{:else}
	<p>just right!</p>
{/if}

{#each expression as name}...
{/each}

{#each expression as name, index}...
{/each}

{#each expression as name, index (key)}...{/each}

{#each expression as name}...
{:else}...
{/each}

<h1>Shopping list</h1>
<ul>
	{#each items as item}
		<li>{item.name} x {item.qty}</li>
	{/each}
</ul>

{#each todos as todo}
	<p>{todo.text}</p>
{:else}
	<p>No tasks today!</p>
{/each}

{#await promise}
	<!-- promise is pending -->
	<p>waiting for the promise to resolve...</p>
{:then value}
	<!-- promise was fulfilled -->
	<p>The value is {value}</p>
{:catch error}
	<!-- promise was rejected -->
	<p>Something went wrong: {error.message}</p>
{/await}

{#key value}
	<div transition:fade>{value}</div>
{/key}


{@debug user}
<!-- Compiles -->
{@debug user}
{@debug user1, user2, user3}

<!-- WON'T compile -->
{@debug user.firstname}
{@debug myArray[0]}
{@debug !isReady}
{@debug typeof user === 'object'}

<form on:submit|preventDefault={handleSubmit}>
	<!-- the `submit` event's default is prevented,
		 so the page won't reload -->
</form>

The following modifiers are available:

    preventDefault — calls event.preventDefault() before running the handler
    stopPropagation — calls event.stopPropagation(), preventing the event reaching the next element
    passive — improves scrolling performance on touch/wheel events (Svelte will add it automatically where it's safe to do so)
    nonpassive — explicitly set passive: false
    capture — fires the handler during the capture phase instead of the bubbling phase
    once — remove the handler after the first time it runs
    self — only trigger handler if event.target is the element itself

<select bind:value={selected}>
	<option value={a}>a</option>
	<option value={b}>b</option>
	<option value={c}>c</option>
</select>

<video
	src={clip}
	bind:duration
	bind:buffered
	bind:played
	bind:seekable
	bind:seeking
	bind:ended
	bind:currentTime
	bind:playbackRate
	bind:paused
	bind:volume
	bind:muted
	bind:videoWidth
	bind:videoHeight
></video>

<canvas bind:this={canvasElement}></canvas>
<div class="{active ? 'active' : ''}">...</div>
<div class:active={active}>...</div>

<div use:foo={bar}></div>

{#if visible}
	<p
		transition:fly="{{ y: 200, duration: 2000 }}"
		on:introstart="{() => status = 'intro started'}"
		on:outrostart="{() => status = 'outro started'}"
		on:introend="{() => status = 'intro ended'}"
		on:outroend="{() => status = 'outro ended'}"
	>
		Flies in and out
	</p>
{/if}

{#if visible}
	<div in:fly out:fade>
		flies in, fades out
	</div>
{/if}

<SomeComponent on:whatever={handler}/>
<button on:click={() => cart.empty()}>
	Empty shopping cart
</button>

<div>
	<slot>
		this fallback content will be rendered when no content is provided, like in the first example
	</slot>
</div>

<!-- App.svelte -->
<Widget></Widget> <!-- this component will render the default content -->

<Widget>
	<p>this is some child content that will overwrite the default slot content</p>
</Widget>

<!-- App.svelte -->
<Widget>
	<HeaderComponent slot="header" />
	<svelte:fragment slot="footer">
		<p>All rights reserved.</p>
		<p>Copyright (c) 2019 Svelte Industries</p>
	</svelte:fragment>
</Widget>

<!-- Card.svelte -->
<div>
	<slot name="title"></slot>
	{#if $$slots.description}
		<!-- This <hr> and slot will render only if a slot named "description" is provided. -->
		<hr>
		<slot name="description"></slot>
	{/if}
</div>

<!-- App.svelte -->
<Card>
	<h1 slot="title">Blog Post Title</h1>
	<!-- No slot named "description" was provided so the optional slot will not be rendered. -->
</Card>

{#if count > 0}
	<p>counting down... {count}</p>
	<svelte:self count="{count - 1}"/>
{:else}
	<p>lift-off!</p>
{/if}
<svelte:component this={currentSelection.component} foo={bar}/>

<svelte:body
	on:mouseenter={handleMouseenter}
	on:mouseleave={handleMouseleave}
/>

