```{=html}
<div class="blog-list">
<% for (const item of items) { %>
  <% if (!item.title || !item.date) throw new Error('Every published blog post needs a title and an explicit publication date: ' + item.path); %>
  <article class="blog-entry<%= item.image ? '' : ' blog-entry-no-image' %>">
    <% if (item.image) { %>
    <div class="blog-thumbnail">
      <img src="<%- item.image %>" alt="" loading="lazy" decoding="async">
    </div>
    <% } %>
    <div class="blog-copy">
      <h2 class="no-anchor"><a href="<%- item.path %>"><%- item.title %></a></h2>
      <p class="blog-date"><%- item.date %></p>
      <% if (item.description) { %>
      <div class="blog-description"><%= item.description %></div>
      <% } %>
      <% if (item.categories && item.categories.length) { %>
      <ul class="blog-topics" aria-label="Topics">
        <% for (const category of item.categories) { %>
        <li><%- category %></li>
        <% } %>
      </ul>
      <% } %>
      <a class="blog-read" href="<%- item.path %>">Read post <span aria-hidden="true">→</span><span class="visually-hidden">: <%- item.title %></span></a>
    </div>
  </article>
<% } %>
</div>
```
