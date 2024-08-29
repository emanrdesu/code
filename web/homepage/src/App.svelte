<script>
  import Icon from "./Icon.svelte";
  import Search from "./Search.svelte";
  import Section from "./Section.svelte";

  const data = [
    {
      icon: "chan",
      color: "#00e68a",
      links: [
        { href: "https://4chan.org/c", text: "/c/" },
        { href: "https://4chan.org/g", text: "/g/" },
        { href: "https://4chan.org/wg", text: "/wg/" },
        { href: "https://4chan.org/r9k", text: "/r9k/" },
        { href: "https://4chan.org/pol", text: "/pol/" },
        { href: "https://4chan.org/fit", text: "/fit/" },
        { href: "https://4chan.org/wsg", text: "/wsg/" },
        { href: "https://4chan.org/tv", text: "/tv/" },
        { href: "https://4chan.org/t", text: "/t/" },
        { href: "https://lainchan.org/%CE%BB/index.html", text: "/λ/" },
      ],
    },
    {
      icon: "person",
      color: "rgb(147, 197, 253)",
      links: [
        { href: "https://app.dataannotation.tech/users/sign_in", text: "work" },
        { href: "https://mail.cock.li", text: "mail" },
        { href: "http://192.168.0.1", text: "router" },
      ],
    },

    {
      icon: "hue",
      color: "rgb(216, 180, 254)",
      links: [
        { href: "https://hues.kepstin.ca", text: "hues" },
        { href: "https://spook.mon.im/", text: "spook" },
        { href: "https://0x40.mon.im/", text: "mon" },
        { href: "https://420.mon.im/snoop.html", text: "420" },
      ],
    },

    {
      icon: "anon",
      color: "rgb(249, 168, 212)",
      links: [
        { href: "https://xvideos.com", text: "xvideos" },
        { href: "https://4chan.org/gif", text: "/gif/" },
        { href: "https://4chan.org/s", text: "/s/" },
      ],
    },

    {
      icon: "rumble",
      color: "#85C742",

      links: [
        { href: "https://rumble.com/c/sneako", text: "sneako" },
        {
          href: "https://rumble.com/c/nickjfuentes/videos",
          text: "nick fuentes",
        },
        {
          href: "https://www.youtube.com/@MentalOutlaw/videos",
          text: "mental outlaw",
        },
      ],
    },
  ];

  const icons = [
    { icon: "whatsapp", color: "#25D366", link: "https://web.whatsapp.com" },
    { icon: "gemini", color: "#7675C8", link: "https://gemini.google.com" },
    { icon: "openai", color: "#b3b3cc", link: "https://chat.openai.com/chat" },
    { icon: "chase", color: "#1680e9", link: "https://chase.com" },
  ];
</script>

<svelte:head>
  <title>Home</title>
</svelte:head>

<div class="hero pt-8 min-w-44 w-screen h-screen">
  <div class="hero-content justify-center text-center flex flex-col">
    {#if navigator.onLine}
      <small class="fixed right-3 bottom-2 italic">
        {#await fetch("http://ifconfig.me/ip").then((r) => r.text()) then ip}
          {ip}
        {/await}
      </small>
    {/if}

    <div class="flex flex-wrap gap-8 mb-6">
      {#each icons as { icon, color, link }}
        <Icon {icon} {color} {link}></Icon>
      {/each}
    </div>

    <span class="w-full">
      <Search />
    </span>

    <div class="section-container">
      <div id="chan">
        <Section icon={data[0].icon} color={data[0].color} links={data[0].links}
        ></Section>
      </div>
      {#each data.slice(1) as { color, icon, links }}
        <Section {color} {icon} {links}></Section>
      {/each}
    </div>
  </div>
</div>

<style>
  .section-container {
    display: grid;
    grid-template-columns: 1fr;
    align-items: stretch;
    margin-left: auto;
    margin-right: auto;
    gap: 1rem;
    padding: 2rem;
    max-width: 80ch;
  }

  @media (min-width: 45ch) {
    .section-container {
      grid-template-columns: 1fr 1fr;
    }
  }

  @media (min-width: 63ch) {
    .section-container {
      grid-template-columns: repeat(3, 1fr);
    }

    #chan {
      grid-row: span 2;
    }
  }
</style>
