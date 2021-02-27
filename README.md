**非アクティブ、参照用**

# slacknowledge


- いわゆる従来的なWEBサービスの構成をしてみたかった(SPAでない)

# 構成(ざっくり)

- BE
  - web/api framewor: servant
  - rdb query builder: HRR
  - elasticsearch client: bloodhound
  - templateengine: heterocephalus + pug
- client side
  - elm
- DB
  - postgres
  - elasticsearch
