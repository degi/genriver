---
layout: home
---
<h1>GenRiver3 Documentation</h1>  

ini adalah sesuatu

{% assign image_files = site.static_files | where: "image", true %}
{% for myimage in image_files %}
  {{ myimage.path }}
{% endfor %}
