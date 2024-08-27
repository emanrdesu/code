<script>
  let value = "";

  function performSearch() {
    if (value) {
      const search = value;
      value = "";
      window.location.href = `https://duckduckgo.com?q=${encodeURIComponent(search)}`;
    }
  }
</script>

<div>
  <input
    on:keydown={(e) => {
      if (e.key == "Enter") {
        performSearch();
      }
    }}
    name="search"
    type="text"
    bind:value
    placeholder="search the web"
  />
  <button on:click={() => performSearch()} class="-translate-x-[1px] px-1">
    <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" class="w-8 h-8">
      <path
        d="M18.031 16.6168L22.3137 20.8995L20.8995 22.3137L16.6168 18.031C15.0769 19.263 13.124 20 11 20C6.032 20 2 15.968 2 11C2 6.032 6.032 2 11 2C15.968 2 20 6.032 20 11C20 13.124 19.263 15.0769 18.031 16.6168ZM16.0247 15.8748C17.2475 14.6146 18 12.8956 18 11C18 7.1325 14.8675 4 11 4C7.1325 4 4 7.1325 4 11C4 14.8675 7.1325 18 11 18C12.8956 18 14.6146 17.2475 15.8748 16.0247L16.0247 15.8748Z"
        fill="#efeff1"
      ></path>
    </svg>
  </button>
</div>

<style>
  div {
    display: flex;
    justify-content: center;
    align-items: center;
    height: 35px;
  }

  input {
    outline: none;
    height: 100%;
    width: 82.3%;
    font-size: 14px;
    font-weight: 500;
    background-color: #53535f;
    text-align: center;
    padding: 7px 10px;
    border: 2px solid transparent;
    border-top-left-radius: 7px;
    border-bottom-left-radius: 7px;
    margin-right: 1px;
    transition: all 0.2s ease;
  }

  input:hover {
    border: 2px solid rgba(255, 255, 255, 0.16);
  }

  input:focus {
    border: 2px solid #966ed3;
    background-color: var(--base-bg-200);
  }

  button {
    flex-grow: 0;
    height: 100%;
    width: 34px;
    border: none;
    cursor: pointer;
    background-color: rgba(54, 54, 56, 1);
    border-top-right-radius: 7px;
    border-bottom-right-radius: 7px;
    padding: 2px 8px;
    display: flex;
    justify-content: center;
    align-items: center;
  }

  button:active,
  button:focus,
  button:hover {
    background-color: rgb(99, 24, 105);
  }
</style>
