<script>
  import eyecon from "./icon";
  export let icon;
  export let color;
  export let links;

  let hover = false;
</script>

<div
  tabindex={hover ? -1 : 0}
  on:mouseenter={() => (hover = true)}
  on:mouseleave={() => (hover = false)}
  style:color
  class="card opacity-70 hover:opacity-100 p-2"
>
  <div class="icon">
    {@html eyecon[icon]}
  </div>
  <nav class="flex outline-inherit flex-col gap-1 justify-center">
    {#each links as { href, text }}
      {@const klass =
        "hover:opacity-100 oscilla hover:text-inherit focus:text-inherit focus:opacity-100 text-base-content opacity-70"}
      <a tabindex={hover ? 0 : -1} {href} class={klass}>{text}</a>
    {/each}
  </nav>
</div>

<style>
  .card {
    background-color: #36384a;
    width: 100%;
    min-width: 17ch;
    min-height: 15ch;
    text-align: center;

    padding-block: 1.5em;
    border-radius: 5px;
    position: relative;
    overflow: hidden;
    display: flex;
    flex-direction: column;
    justify-content: center;
    align-items: center;
    gap: 1em;
  }

  .card nav {
    width: 100%;
    height: 100%;
    font-weight: bold;
    opacity: 0;
    transition: opacity 250ms cubic-bezier(0.6, 0.4, 0, 1);
  }

  .card:hover nav {
    opacity: 1;
  }

  .card .icon {
    position: absolute;
    transition: opacity 250ms cubic-bezier(0.6, 0.4, 0, 1);
  }

  .card:hover .icon {
    opacity: 0;
  }
</style>
