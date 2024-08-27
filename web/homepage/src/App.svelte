<script>
  import Section from "./Section.svelte";
  const size = 42;

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
        { href: "https://chase.com", text: "bank" },
        { href: "https://chat.openai.com/chat", text: "chatgpt" },
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
        { href: "https://horror.0x40hu.es", text: "horror" },
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
</script>

<svelte:head>
  <title>Home</title>
</svelte:head>

<div class="hero min-w-44 w-screen h-screen">
  <div class="hero-content text-center">
    <div>
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
  div.hero-content > div {
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
    div.hero-content > div {
      grid-template-columns: 1fr 1fr;
    }
  }

  @media (min-width: 63ch) {
    div.hero-content > div {
      grid-template-columns: repeat(3, 1fr);
    }

    #chan {
      grid-row: span 2;
    }
  }
</style>
