<script>
  import ikon from "./icon.js";

  export let icon;
  export let color;
  export let link;
</script>

<div class="oscilla" style:color>
  <a class="rounded-lg outline-1" style:border-color={color} href={link}>
    {@html ikon[icon]}
  </a>
</div>
