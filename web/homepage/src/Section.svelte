<script>
  import eyecon from "./icon";
  export let icon;
  export let color;
  export let links;
</script>

<div style:color class="card opacity-70 hover:opacity-100 p-2">
  <div class="icon">
    {@html eyecon[icon]}
  </div>
  <nav class="flex flex-col gap-1 justify-center">
    {#each links as { href, text }}
      <a
        {href}
        class="hover:opacity-100 hover:text-inherit text-base-content opacity-70"
        >{text}</a
      >
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
    transition:
      0.3s cubic-bezier(0.6, 0.4, 0, 1),
      transform 0.2s ease;
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
    transition: all 0.3s cubic-bezier(0.6, 0.4, 0, 1);
  }

  .card:hover nav {
    opacity: 1;
  }

  .card .icon {
    position: absolute;
    transition: all 0.3s cubic-bezier(0.6, 0.4, 0, 1);
  }

  nav a:hover {
    animation: oscillate 700ms ease-in-out infinite;
  }

  @keyframes oscillate {
    0% {
      opacity: 1;
    }
    50% {
      opacity: 0.5;
    }
    100% {
      opacity: 1;
    }
  }

  .card:hover .icon {
    opacity: 0;
  }
</style>
