# REmojiTree
This is an R script that builds a phylogenetic tree of all existing plant emojis.

It is inspired from [PlantPhylomoji](https://github.com/ghuertaramos/PlantPhylomoji). As many recently added emojis are missing from the emojifont package used by PlantPhylomoji, it does not allow to build trees with emojis that were added later than 2016.
To resolve this, REmojiTree downloads images of the emojis from https://emojipedia.org/ to build the tree.
You will need to define the folder you want to download the images to in the imgdir variable.
The [ggplot2](https://ggplot2.tidyverse.org/) and [ggtree](https://bioconductor.org/packages/release/bioc/html/ggtree.html) packages are required.


REmojiTree supports Apple, Twitter, Facebook, Google, Whatsapp and Samsung type emojis. You will need to define the type you want in the emoji_type variable.

These are the trees that can be built with REmojiTree:

## Apple
<p align="center">  
<img width="500" height="500" src="./Trees/emojitree_Apple.png">
</p>

## Google
<p align="center">  
<img width="500" height="500" src="./Trees/emojitree_Google.png">
</p>

## Samsung
<p align="center">  
<img width="500" height="500" src="./Trees/emojitree_Samsung.png">
</p>

## Twitter
<p align="center">  
<img width="500" height="500" src="./Trees/emojitree_Twitter.png">
</p>

## WhatsApp
<p align="center">  
<img width="500" height="500" src="./Trees/emojitree_WhatsApp.png">
</p>

## Tables of emojis

Some emojis represent different plants depending on the emoji type so the trees will differ. Some choices may be arbitrary. 
Here are the corresponding taxa:

| Emoji | Taxon        | Apple | Facebook | Google | Samsung | Twitter | WhatsApp |
|-------|--------------|--------|---------|---------|----------|---------|----------|
| 🥬   | *Brassica rapa*| <p align="center"><img width="50" height="50" src="./Emojis/Apple/leafy-green.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/leafy-green.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/leafy-green.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Samsung/leafy-green.png"></p> | | |
| 🥬   | *Lactuca sativa*  | | | | | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/leafy-green.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/leafy-green.png"></p> |
| 🪴   | Filicophyta | | | <p align="center"><img width="50" height="50" src="./Emojis/Google/potted-plant.png"></p> | | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/potted-plant.png"></p> | |
| 🪴   | Araceae |  | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/potted-plant.png"></p> |  | <p align="center"><img width="50" height="50" src="./Emojis/Samsung/potted-plant.png"></p> | | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/potted-plant.png"></p> |

Note: the Apple emoji for the potted plant is too generic and no taxon could be clearly attributed.
<p align="left"><img width="50" height="50" src="./Emojis/Apple/potted-plant.png"></p>

Here are the corresponding taxa for the remaining emojis:

| Emoji | Taxon        | Apple | Facebook | Google | Samsung | Twitter | WhatsApp |
|-------|--------------|--------|---------|---------|----------|---------|----------|
| 🌲   | Gymnospermae | <p align="center"><img width="50" height="50" src="./Emojis/Apple/evergreen-tree.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/evergreen-tree.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/evergreen-tree.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/evergreen-tree.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/evergreen-tree.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/evergreen-tree.png"></p> |
| 🎄   |  Gymnospermae | <p align="center"><img width="50" height="50" src="./Emojis/Apple/christmas-tree.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/christmas-tree.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/christmas-tree.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/christmas-tree.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/christmas-tree.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/christmas-tree.png"></p> |
| 🥑   | *Persea americana* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/avocado.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/avocado.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/avocado.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/avocado.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/avocado.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/avocado.png"></p> |
| 🪷   | *Nelumbo* spp.| <p align="center"><img width="50" height="50" src="./Emojis/Apple/lotus.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/lotus.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/lotus.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/lotus.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/lotus.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/lotus.png"></p> |
| 🌷   | *Tulipa* spp.| <p align="center"><img width="50" height="50" src="./Emojis/Apple/tulip.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/tulip.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/tulip.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/tulip.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/tulip.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/tulip.png"></p> |
| 🪻   | *Hyacinthus * spp. |  <p align="center"><img width="50" height="50" src="./Emojis/Apple/hyacinth.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/hyacinth.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/hyacinth.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/hyacinth.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/hyacinth.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/hyacinth.png"></p> |
| 🧄   | *Allium sativum* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/garlic.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/garlic.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/garlic.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/garlic.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/garlic.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/garlic.png"></p> |
| 🧅   | *Allium cepa* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/onion.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/onion.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/onion.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/onion.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/onion.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/onion.png"></p> |
| 🌴   | Arecaceae | <p align="center"><img width="50" height="50" src="./Emojis/Apple/palm-tree.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/palm-tree.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/palm-tree.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/palm-tree.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/palm-tree.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/palm-tree.png"></p> |
| 🥥   | *Cocos nucifera* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/coconut.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/coconut.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/coconut.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/coconut.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/coconut.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/coconut.png"></p> |
| 🍌   | *Musa* spp.| <p align="center"><img width="50" height="50" src="./Emojis/Apple/banana.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/banana.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/banana.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/banana.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/banana.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/banana.png"></p> |
| 🫚   | *Zingiber officinale* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/ear-of-corn.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/ear-of-corn.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/ear-of-corn.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/ear-of-corn.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/ear-of-corn.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/ear-of-corn.png"></p> |
| 🍍   | *Ananas comosus* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/pineapple.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/pineapple.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/pineapple.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/pineapple.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/pineapple.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/pineapple.png"></p> |
| 🌽   | *Zea mays* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/ear-of-corn.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/ear-of-corn.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/ear-of-corn.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/ear-of-corn.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/ear-of-corn.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/ear-of-corn.png"></p> |
| 🎋   | Bambusoideae | <p align="center"><img width="50" height="50" src="./Emojis/Apple/tanabata-tree.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/tanabata-tree.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/tanabata-tree.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/tanabata-tree.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/tanabata-tree.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/tanabata-tree.png"></p> |
|  🌾 | *Oryza sativa* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/sheaf-of-rice.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/sheaf-of-rice.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/sheaf-of-rice.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/sheaf-of-rice.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/sheaf-of-rice.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/sheaf-of-rice.png"></p> |
| 🌵   | Cactaceae | <p align="center"><img width="50" height="50" src="./Emojis/Apple/cactus.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/cactus.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/cactus.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/cactus.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/cactus.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/cactus.png"></p> |
| 🍵   | *Camellia sinensis* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/teacup-without-handle.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/teacup-without-handle.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/teacup-without-handle.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/teacup-without-handle.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/teacup-without-handle.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/teacup-without-handle.png"></p> |
| 🥝   | *Actinidia* spp.| <p align="center"><img width="50" height="50" src="./Emojis/Apple/kiwi-fruit.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/kiwi-fruit.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/kiwi-fruit.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/kiwi-fruit.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/kiwi-fruit.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/kiwi-fruit.png"></p> |
| 🫐   | *Vaccinium* spp.| <p align="center"><img width="50" height="50" src="./Emojis/Apple/blueberries.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/blueberries.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/blueberries.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/blueberries.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/blueberries.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/blueberries.png"></p> |
| 🥕   | *Daucus carota* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/carrot.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/carrot.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/carrot.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/carrot.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/carrot.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/carrot.png"></p> |
| 🌻   | *Helianthus annuus* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/sunflower.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/sunflower.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/sunflower.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Samsung/sunflower.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/sunflower.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/sunflower.png"></p> |
| 🌼   | Asteraceae | <p align="center"><img width="50" height="50" src="./Emojis/Apple/blossom.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/blossom.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/blossom.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Samsung/blossom.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/blossom.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/blossom.png"></p> |
| 🫒   | *Olea europaea* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/olive.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/olive.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/olive.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/olive.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/olive.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/olive.png"></p> |
| ☕  | *Coffea* spp. | <p align="center"><img width="50" height="50" src="./Emojis/Apple/hot-beverage.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/hot-beverage.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/hot-beverage.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/hot-beverage.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/hot-beverage.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/hot-beverage.png"></p> |
| 🍠   | *Ipomea patatas* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/roasted-sweet-potato.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/roasted-sweet-potato.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/roasted-sweet-potato.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/roasted-sweet-potato.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/roasted-sweet-potato.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/roasted-sweet-potato.png"></p> |
| 🚬   | *Nicotiana tabacum* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/cigarette.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/cigarette.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/cigarette.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/cigarette.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/cigarette.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/cigarette.png"></p> |
| 🌶️🫑 | *Capsicum* spp. | <p align="center"><img width="50" height="50" src="./Emojis/Apple/bell-pepper.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/bell-pepper.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/bell-pepper.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/bell-pepper.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/bell-pepper.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/bell-pepper.png"></p> |
| 🍆   | *Solanum melongena* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/eggplant.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/eggplant.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/eggplant.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/eggplant.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/eggplant.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/eggplant.png"></p> |
| 🥔   | *Solanum tuberosum* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/potato.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/potato.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/potato.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/potato.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/potato.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/potato.png"></p> |
| 🍅   | *Solanum lycopersicum* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/tomato.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/tomato.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/tomato.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/tomato.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/tomato.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/tomato.png"></p> |
| 🍇   | *Vitis vinifera* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/grapes.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/grapes.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/grapes.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/grapes.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/grapes.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/grapes.png"></p> |
| 🥦   | *Brassica oleracea* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/broccoli.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/broccoli.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/broccoli.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/broccoli.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/broccoli.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/broccoli.png"></p> |
| 🌺   | *Hibiscus* spp.| <p align="center"><img width="50" height="50" src="./Emojis/Apple/hibiscus.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/hibiscus.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/hibiscus.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/hibiscus.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/hibiscus.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/hibiscus.png"></p> |
| 🍫   | *Theobroma cacao* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/chocolate-bar.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/chocolate-bar.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/chocolate-bar.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/chocolate-bar.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/chocolate-bar.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/chocolate-bar.png"></p> |
| 🥭   | *Mangifera indica* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/mango.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/mango.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/mango.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/mango.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/mango.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/mango.png"></p> |
| 🍁   | *Acer* spp. | <p align="center"><img width="50" height="50" src="./Emojis/Apple/maple-leaf.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/maple-leaf.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/maple-leaf.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/maple-leaf.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/maple-leaf.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/maple-leaf.png"></p> |
| 🍊   | *Citrus reticulata* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/tangerine.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/tangerine.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/tangerine.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/tangerine.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/tangerine.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/tangerine.png"></p> |
| 🍋   | *Citrus limon* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/lemon.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/lemon.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/lemon.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/lemon.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/lemon.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/lemon.png"></p> |
| ☘️🍀   | *Trifolium* spp.| <p align="center"><img width="50" height="50" src="./Emojis/Apple/shamrock.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/shamrock.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/shamrock.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/shamrock.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/shamrock.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/shamrock.png"></p> |
| 🥜   | *Arachis hypogaea* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/peanuts.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/peanuts.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/peanuts.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/peanuts.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/peanuts.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/peanuts.png"></p> |
| 🫘   | Fabaceae | <p align="center"><img width="50" height="50" src="./Emojis/Apple/beans.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/beans.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/beans.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/beans.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/beans.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/beans.png"></p> |
| 🫛   | *Pisum sativum* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/pea-pod.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/pea-pod.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/pea-pod.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/pea-pod.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/pea-pod.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/pea-pod.png"></p> |
| 🌰   | *Castanea* spp.| <p align="center"><img width="50" height="50" src="./Emojis/Apple/chestnut.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/chestnut.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/chestnut.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/chestnut.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/chestnut.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/chestnut.png"></p> |
| 🎃   | *Cucurbita pepo* subsp. *pepo*| <p align="center"><img width="50" height="50" src="./Emojis/Apple/jack-o-lantern.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/jack-o-lantern.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/jack-o-lantern.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/jack-o-lantern.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/jack-o-lantern.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/jack-o-lantern.png"></p> |
| 🍉   | *Citrullus lanatus * | <p align="center"><img width="50" height="50" src="./Emojis/Apple/watermelon.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/watermelon.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/watermelon.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/watermelon.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/watermelon.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/watermelon.png"></p> |
| 🥒   | *Cucumis sativus* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/cucumber.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/cucumber.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/cucumber.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/cucumber.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/cucumber.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/cucumber.png"></p> |
| 🍈   | *Cucumis melo* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/melon.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/melon.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/melon.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/melon.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/melon.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/melon.png"></p> |
| 🌹   | *Rosa* spp.| <p align="center"><img width="50" height="50" src="./Emojis/Apple/rose.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/rose.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/rose.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/rose.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/rose.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/rose.png"></p> |
| 🍓   | *Fragaria ×ananassa*| <p align="center"><img width="50" height="50" src="./Emojis/Apple/strawberry.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/strawberry.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/strawberry.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/strawberry.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/strawberry.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/strawberry.png"></p> |
| 🍐   | *Pyrus communis* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/pear.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/pear.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/pear.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/pear.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/pear.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/pear.png"></p> |
| 🍎🍏 | *Malus* spp. | <p align="center"><img width="50" height="50" src="./Emojis/Apple/red-apple.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/red-apple.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/red-apple.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/red-apple.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/red-apple.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/red-apple.png"></p> |
| 🍑   | *Prunus* spp.| <p align="center"><img width="50" height="50" src="./Emojis/Apple/peach.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/peach.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/peach.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/peach.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/peach.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/peach.png"></p> |
| 🌸   | *Prunus* spp.| <p align="center"><img width="50" height="50" src="./Emojis/Apple/cherry-blossom.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/cherry-blossom.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/cherry-blossom.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/cherry-blossom.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/cherry-blossom.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/cherry-blossom.png"></p> |
| 🍒   | *Prunus* subg. *Cerasus* | <p align="center"><img width="50" height="50" src="./Emojis/Apple/cherries.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Facebook/cherries.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Google/cherries.png"></p >| <p align="center"><img width="50" height="50" src="./Emojis/Samsung/cherries.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/Twitter/cherries.png"></p> | <p align="center"><img width="50" height="50" src="./Emojis/WhatsApp/cherries.png"></p> |
