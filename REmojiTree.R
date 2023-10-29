library(ggplot2)
library(ggtree)

#Define the directory you want your images in
imgdir <- c("C:/")
#Select type of emoji you want ("Apple", "Twitter", "Facebook", "Google", "WhatsApp", "Samsung")
emoji_type = "Samsung"


#Define names
names <- c(
  "evergreen-tree_1f332", "avocado_1f951", "tulip_1f337", "palm-tree_1f334", "banana_1f34c",
  "pineapple_1f34d", "ear-of-corn_1f33d", "tanabata-tree_1f38b", "sheaf-of-rice_1f33e", "grapes_1f347",
  "peanuts_1f95c", "shamrock_2618-fe0f", "rose_1f339", "strawberry_1f353", "red-apple_1f34e", 
  "pear_1f350", "peach_1f351", "cherry-blossom_1f338", "cherries_1f352", "chestnut_1f330",
  "jack-o-lantern_1f383", "watermelon_1f349", "cucumber_1f952", "melon_1f348", "hibiscus_1f33a",
  "tangerine_1f34a", "lemon_1f34b", "maple-leaf_1f341", "cactus_1f335", "kiwi-fruit_1f95d", 
  "roasted-sweet-potato_1f360", "hot-pepper_1f336-fe0f", "eggplant_1f346", "potato_1f954", "tomato_1f345",
  "carrot_1f955", "sunflower_1f33b", "blossom_1f33c", "green-apple_1f34f", "christmas-tree_1f384",
  "coconut_1f965", "garlic_1f9c4", "ginger_1fada", "hyacinth_1fabb", "lotus_1fab7", "onion_1f9c5",
  "pea-pod_1fadb", "beans_1fad8", "bell-pepper_1fad1", "blueberries_1fad0", "broccoli_1f966", "chocolate-bar_1f36b",
  "four-leaf-clover_1f340", "leafy-green_1f96c", "mango_1f96d", "olive_1fad2", "cigarette_1f6ac",
  "potted-plant_1fab4", "hot-beverage_2615", "teacup-without-handle_1f375"
)


# Define placeholder URL patterns for each emoji type
url_patterns <- list(
  Apple = "https://em-content.zobj.net/source/apple/354/",
  Twitter = "https://em-content.zobj.net/source/twitter/376/",
  Google = "https://em-content.zobj.net/source/google/350/",
  WhatsApp = "https://em-content.zobj.net/source/whatsapp/352/",
  Facebook = "https://em-content.zobj.net/source/facebook/355/",
  Samsung = "https://em-content.zobj.net/source/samsung/349/"
)

# Use the selected emoji type to fetch the URL
selected_url <- url_patterns[[emoji_type]]

# Download images
for (name in names) {
  image_url <- paste0(selected_url, name, ".png")
  simplified_filename <- sub("_[0-9a-fA-F-]+$", ".png", name)
  local_filename <- file.path(imgdir, simplified_filename, fsep = .Platform$file.sep)
  download.file(image_url, destfile = local_filename, mode = "wb")
  cat("Downloaded", simplified_filename, "emoji.\n")
}

# Generate Newick tree string based on emoji type
generate_tree <- function(emoji_type) {
  if (emoji_type == "Twitter") {
    nwk <- paste0("(potted-plant, ((evergreen-tree, christmas-tree),((avocado, lotus),((tulip, ((hyacinth, (garlic, onion)), ((palm-tree, coconut), ((banana, ginger), (pineapple, (ear-of-corn, (tanabata-tree, sheaf-of-rice))))))),((grapes, ((((peanuts, (beans, pea-pod)), (shamrock, four-leaf-clover)), (((rose, strawberry), (((red-apple, green-apple), pear), (peach, (cherry-blossom, cherries)))), (chestnut, (jack-o-lantern, (watermelon, (cucumber, melon)))))), (((hibiscus, chocolate-bar), broccoli), ((mango, ((tangerine, lemon), (maple-leaf))))))),(cactus, ((teacup-without-handle, (kiwi-fruit, blueberries)), ((olive, hot-beverage,(roasted-sweet-potato,(cigarette, ((hot-pepper, bell-pepper), (eggplant, (potato, tomato)))))), (carrot, (leafy-green, (sunflower, blossom)))))))))));")
  } else if (emoji_type == "WhatsApp") {
    nwk <- paste0("((evergreen-tree, christmas-tree),((avocado, lotus),(((potted-plant, (tulip, ((hyacinth, (garlic, onion)), ((palm-tree, coconut), ((banana, ginger), (pineapple, (ear-of-corn, (tanabata-tree, sheaf-of-rice))))))))),((grapes, ((((peanuts, (beans, pea-pod)), (shamrock, four-leaf-clover)), (((rose, strawberry), (((red-apple, green-apple), pear), (peach, (cherry-blossom, cherries)))), (chestnut, (jack-o-lantern, (watermelon, (cucumber, melon)))))), (((hibiscus, chocolate-bar), broccoli), ((mango, ((tangerine, lemon), (maple-leaf))))))),(cactus, ((teacup-without-handle, (kiwi-fruit, blueberries)), ((olive, hot-beverage,(roasted-sweet-potato,(cigarette, ((hot-pepper, bell-pepper), (eggplant, (potato, tomato)))))), (carrot, (leafy-green, (sunflower, blossom))))))))));")
  } else if (emoji_type == "Apple") {
    nwk <- paste0("((evergreen-tree, christmas-tree),((avocado, lotus),(((tulip, ((hyacinth, (garlic, onion)), ((palm-tree, coconut), ((banana, ginger), (pineapple, (ear-of-corn, (tanabata-tree, sheaf-of-rice)))))))),((grapes, ((((peanuts, (beans, pea-pod)), (shamrock, four-leaf-clover)), (((rose, strawberry), (((red-apple, green-apple), pear), (peach, (cherry-blossom, cherries)))), (chestnut, (jack-o-lantern, (watermelon, (cucumber, melon)))))), (((hibiscus, chocolate-bar), (broccoli, leafy-green)), ((mango, ((tangerine, lemon), (maple-leaf))))))),(cactus, ((teacup-without-handle, (kiwi-fruit, blueberries)), ((olive, hot-beverage,(roasted-sweet-potato,(cigarette, ((hot-pepper, bell-pepper), (eggplant, (potato, tomato)))))), (carrot, (sunflower, blossom)))))))));")
  } else if (emoji_type == "Google") {
    nwk <- paste0("(potted-plant, ((evergreen-tree, christmas-tree),((avocado, lotus),(((tulip, ((hyacinth, (garlic, onion)), ((palm-tree, coconut), ((banana, ginger), (pineapple, (ear-of-corn, (tanabata-tree, sheaf-of-rice)))))))),((grapes, ((((peanuts, (beans, pea-pod)), (shamrock, four-leaf-clover)), (((rose, strawberry), (((red-apple, green-apple), pear), (peach, (cherry-blossom, cherries)))), (chestnut, (jack-o-lantern, (watermelon, (cucumber, melon)))))), (((hibiscus, chocolate-bar), (broccoli, leafy-green)), ((mango, ((tangerine, lemon), (maple-leaf))))))),(cactus, ((teacup-without-handle, (kiwi-fruit, blueberries)), ((olive, hot-beverage,(roasted-sweet-potato,(cigarette, ((hot-pepper, bell-pepper), (eggplant, (potato, tomato)))))), (carrot, (sunflower, blossom))))))))));")
  } else if (emoji_type == "Facebook") {
    nwk <- paste0("((evergreen-tree, christmas-tree),((avocado, lotus),(((potted-plant, (tulip, ((hyacinth, (garlic, onion)), ((palm-tree, coconut), ((banana, ginger), (pineapple, (ear-of-corn, (tanabata-tree, sheaf-of-rice))))))))),((grapes, ((((peanuts, (beans, pea-pod)), (shamrock, four-leaf-clover)), (((rose, strawberry), (((red-apple, green-apple), pear), (peach, (cherry-blossom, cherries)))), (chestnut, (jack-o-lantern, (watermelon, (cucumber, melon)))))), (((hibiscus, chocolate-bar), (broccoli,leafy-green)), ((mango, ((tangerine, lemon), (maple-leaf))))))),(cactus, ((teacup-without-handle, (kiwi-fruit, blueberries)), ((olive, hot-beverage,(roasted-sweet-potato,(cigarette, ((hot-pepper, bell-pepper), (eggplant, (potato, tomato)))))), (carrot, (sunflower, blossom)))))))));")
  } else if (emoji_type == "Samsung") {
    nwk <- paste0("((evergreen-tree, christmas-tree),((avocado, lotus),(((potted-plant, (tulip, ((hyacinth, (garlic, onion)), ((palm-tree, coconut), ((banana, ginger), (pineapple, (ear-of-corn, (tanabata-tree, sheaf-of-rice))))))))),((grapes, ((((peanuts, (beans, pea-pod)), (shamrock, four-leaf-clover)), (((rose, strawberry), (((red-apple, green-apple), pear), (peach, (cherry-blossom, cherries)))), (chestnut, (jack-o-lantern, (watermelon, (cucumber, melon)))))), (((hibiscus, chocolate-bar), (broccoli,leafy-green)), ((mango, ((tangerine, lemon), (maple-leaf))))))),(cactus, ((teacup-without-handle, (kiwi-fruit, blueberries)), ((olive, hot-beverage,(roasted-sweet-potato,(cigarette, ((hot-pepper, bell-pepper), (eggplant, (potato, tomato)))))), (carrot, (sunflower, blossom)))))))));")
  }
  return(nwk)
}

# Get Newick tree string based on emoji type
nwk <- generate_tree(emoji_type)
tree = read.tree(text = nwk)

p <- ggtree(tree, layout = "circular") +
  geom_tiplab(aes(image=paste0(imgdir, '/', label, '.png')), geom="image", offset=3, align = T, size=.05)

#save as png
png(filename = file.path(imgdir, "emojitree.png"), width = 10, height = 10, unit = "cm", res = 1000)
p
dev.off()
