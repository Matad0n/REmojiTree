library(ggplot2)
library(ggtree)

#Define the directory you want your images in
imgdir <- c("C:/")
#Select type of emoji you want ("Apple", "Twitter", "Facebook", "Google", "WhatsApp", "Samsung")
emoji_type = "Samsung"
#Select the type of tree you want ("Animal", "Plant", "All")
treetype = "All"

if ( treetype == "Plant" ) {
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
    download.file(image_url, destfile = local_filename, mode = "wb", quiet = TRUE)
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
} else if (treetype == "Animal") {
  
  #Define names
  names <- c(
    "monkey_1f412", "gorilla_1f98d", "orangutan_1f9a7", "dog_1f415", "wolf_1f43a",
    "fox_1f98a", "raccoon_1f99d", "cat_1f408", "lion_1f981", "tiger_1f405", "leopard_1f406",
    "moose_1face", "horse_1f40e", "zebra_1f993", "deer_1f98c", "bison_1f9ac", "ox_1f402",
    "water-buffalo_1f403", "cow_1f404", "pig_1f416", "boar_1f417", "ewe_1f411", "goat_1f410",
    "camel_1f42a", "two-hump-camel_1f42b", "llama_1f999", "giraffe_1f992", "elephant_1f418",
    "mammoth_1f9a3", "rhinoceros_1f98f", "hippopotamus_1f99b", "mouse_1f401", "rat_1f400",
    "hamster_1f439", "rabbit_1f407", "chipmunk_1f43f-fe0f", "beaver_1f9ab", "hedgehog_1f994", 
    "bat_1f987", "bear_1f43b", "polar-bear_1f43b-200d-2744-fe0f", "koala_1f428", "panda_1f43c",
    "sloth_1f9a5", "otter_1f9a6", "skunk_1f9a8", "kangaroo_1f998", "badger_1f9a1", "rooster_1f413",
    "penguin_1f427", "dove_1f54a-fe0f", "eagle_1f985", "duck_1f986", "swan_1f9a2", "owl_1f989",
    "turkey_1f983", "dodo_1f9a4", "flamingo_1f9a9", "peacock_1f99a", "parrot_1f99c", 
    "goose_1fabf", "frog_1f438", "crocodile_1f40a", "turtle_1f422", "lizard_1f98e",
    "snake_1f40d", "sauropod_1f995", "t-rex_1f996", "spouting-whale_1f433", "dolphin_1f42c",
    "seal_1f9ad", "fish_1f41f", "tropical-fish_1f420", "blowfish_1f421", "shark_1f988",
    "octopus_1f419", "spiral-shell_1f41a", "coral_1fab8", "jellyfish_1fabc",
    "crab_1f980", "lobster_1f99e", "shrimp_1f990", "squid_1f991", "oyster_1f9aa",
    "snail_1f40c", "butterfly_1f98b", "ant_1f41c", "honeybee_1f41d", "beetle_1fab2",
    "lady-beetle_1f41e", "cricket_1f997", "cockroach_1fab3", "spider_1f577-fe0f",
    "scorpion_1f982", "mosquito_1f99f", "fly_1fab0", "worm_1fab1",
    "bust-in-silhouette_1f464"
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
    download.file(image_url, destfile = local_filename, mode = "wb", quiet = TRUE)
    cat("Downloaded", simplified_filename, "emoji.\n")
  }
  nwk <- paste0("();")
  
  nwk <- paste0("((coral, jellyfish), (((((octopus, squid), (oyster, (snail, spiral-shell))), worm), ((spider, scorpion), ((shrimp, (crab, lobster)), ((cricket, cockroach), ((honeybee, ant), (((beetle, lady-beetle), (butterfly, (mosquito, fly))))))))),
              (shark,((fish, tropical-fish, blowfish), (frog, (((turtle, (crocodile, ((sauropod, (t-rex, ((((rooster, peacock), turkey), ((goose, swan ), duck)), (flamingo, ((dove, dodo), (penguin, (owl, eagle, parrot))))))), (lizard, snake)))),
              ((kangaroo, koala), ((((elephant, mammoth)),sloth), ((hedgehog, (bat, (((((camel, two-hump-camel), llama), ((boar, pig), ((giraffe, ((deer, moose), ((ewe, goat), (water-buffalo, (bison, (ox, cow)))))), (hippopotamus, (dolphin, spouting-whale))))), ((horse, zebra), rhinoceros)),
              (((tiger, (lion, leopard)), cat), (((dog, wolf), fox), (((bear, polar-bear), panda), (seal, (skunk, (raccoon, (badger, otter)))))))))),
              ((rabbit, (chipmunk, (beaver, (hamster, (rat, mouse))))), (monkey, (orangutan, (gorilla, bust-in-silhouette))))))))))))));")
  tree = read.tree(text = nwk)
  p <- ggtree(tree, layout = "circular") +
    geom_tiplab(aes(image=paste0(imgdir, '/', label, '.png')), geom="image", offset=3, align = T, size=.025)
} else if (treetype == "All") {
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
    "potted-plant_1fab4", "hot-beverage_2615", "teacup-without-handle_1f375", 
    "monkey_1f412", "gorilla_1f98d", "orangutan_1f9a7", "dog_1f415", "wolf_1f43a",
    "fox_1f98a", "raccoon_1f99d", "cat_1f408", "lion_1f981", "tiger_1f405", "leopard_1f406",
    "moose_1face", "horse_1f40e", "zebra_1f993", "deer_1f98c", "bison_1f9ac", "ox_1f402",
    "water-buffalo_1f403", "cow_1f404", "pig_1f416", "boar_1f417", "ewe_1f411", "goat_1f410",
    "camel_1f42a", "two-hump-camel_1f42b", "llama_1f999", "giraffe_1f992", "elephant_1f418",
    "mammoth_1f9a3", "rhinoceros_1f98f", "hippopotamus_1f99b", "mouse_1f401", "rat_1f400",
    "hamster_1f439", "rabbit_1f407", "chipmunk_1f43f-fe0f", "beaver_1f9ab", "hedgehog_1f994", 
    "bat_1f987", "bear_1f43b", "polar-bear_1f43b-200d-2744-fe0f", "koala_1f428", "panda_1f43c",
    "sloth_1f9a5", "otter_1f9a6", "skunk_1f9a8", "kangaroo_1f998", "badger_1f9a1", "rooster_1f413",
    "penguin_1f427", "dove_1f54a-fe0f", "eagle_1f985", "duck_1f986", "swan_1f9a2", "owl_1f989",
    "turkey_1f983", "dodo_1f9a4", "flamingo_1f9a9", "peacock_1f99a", "parrot_1f99c", 
    "goose_1fabf", "frog_1f438", "crocodile_1f40a", "turtle_1f422", "lizard_1f98e",
    "snake_1f40d", "sauropod_1f995", "t-rex_1f996", "spouting-whale_1f433", "dolphin_1f42c",
    "seal_1f9ad", "fish_1f41f", "tropical-fish_1f420", "blowfish_1f421", "shark_1f988",
    "octopus_1f419", "spiral-shell_1f41a", "coral_1fab8", "jellyfish_1fabc",
    "crab_1f980", "lobster_1f99e", "shrimp_1f990", "squid_1f991", "oyster_1f9aa",
    "snail_1f40c", "butterfly_1f98b", "ant_1f41c", "honeybee_1f41d", "beetle_1fab2",
    "lady-beetle_1f41e", "cricket_1f997", "cockroach_1fab3", "spider_1f577-fe0f",
    "scorpion_1f982", "mosquito_1f99f", "fly_1fab0", "worm_1fab1",
    "bust-in-silhouette_1f464", "microbe_1f9a0", "mushroom_1f344"
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
    download.file(image_url, destfile = local_filename, mode = "wb", quiet = TRUE)
    cat("Downloaded", simplified_filename, "emoji.\n")
  }
  # Generate Newick tree string based on emoji type
  generate_tree <- function(emoji_type) {
    if (emoji_type == "Twitter") {
      nwk <- paste0("(microbe, ((potted-plant, ((evergreen-tree, christmas-tree),((avocado, lotus),((tulip, ((hyacinth, (garlic, onion)), ((palm-tree, coconut), ((banana, ginger), (pineapple, (ear-of-corn, (tanabata-tree, sheaf-of-rice))))))),((grapes, ((((peanuts, (beans, pea-pod)), (shamrock, four-leaf-clover)), (((rose, strawberry), (((red-apple, green-apple), pear), (peach, (cherry-blossom, cherries)))), (chestnut, (jack-o-lantern, (watermelon, (cucumber, melon)))))), (((hibiscus, chocolate-bar), broccoli), ((mango, ((tangerine, lemon), (maple-leaf))))))),(cactus, ((teacup-without-handle, (kiwi-fruit, blueberries)), ((olive, hot-beverage,(roasted-sweet-potato,(cigarette, ((hot-pepper, bell-pepper), (eggplant, (potato, tomato)))))), (carrot, (leafy-green, (sunflower, blossom))))))))))), 
              (mushroom, ((coral, jellyfish), (((((octopus, squid), (oyster, (snail, spiral-shell))), worm), ((spider, scorpion), ((shrimp, (crab, lobster)), ((cricket, cockroach), ((honeybee, ant), (((beetle, lady-beetle), (butterfly, (mosquito, fly))))))))),
              (shark,((fish, tropical-fish, blowfish), (frog, (((turtle, (crocodile, ((sauropod, (t-rex, ((((rooster, peacock), turkey), ((goose, swan ), duck)), (flamingo, ((dove, dodo), (penguin, (owl, eagle, parrot))))))), (lizard, snake)))),
              ((kangaroo, koala), ((((elephant, mammoth)),sloth), ((hedgehog, (bat, (((((camel, two-hump-camel), llama), ((boar, pig), ((giraffe, ((deer, moose), ((ewe, goat), (water-buffalo, (bison, (ox, cow)))))), (hippopotamus, (dolphin, spouting-whale))))), ((horse, zebra), rhinoceros)),
              (((tiger, (lion, leopard)), cat), (((dog, wolf), fox), (((bear, polar-bear), panda), (seal, (skunk, (raccoon, (badger, otter)))))))))),
              ((rabbit, (chipmunk, (beaver, (hamster, (rat, mouse))))), (monkey, (orangutan, (gorilla, bust-in-silhouette)))))))))))))))));")
    } else if (emoji_type == "WhatsApp") {
      nwk <- paste0("(microbe, (((evergreen-tree, christmas-tree),((avocado, lotus),(((potted-plant, (tulip, ((hyacinth, (garlic, onion)), ((palm-tree, coconut), ((banana, ginger), (pineapple, (ear-of-corn, (tanabata-tree, sheaf-of-rice))))))))),((grapes, ((((peanuts, (beans, pea-pod)), (shamrock, four-leaf-clover)), (((rose, strawberry), (((red-apple, green-apple), pear), (peach, (cherry-blossom, cherries)))), (chestnut, (jack-o-lantern, (watermelon, (cucumber, melon)))))), (((hibiscus, chocolate-bar), broccoli), ((mango, ((tangerine, lemon), (maple-leaf))))))),(cactus, ((teacup-without-handle, (kiwi-fruit, blueberries)), ((olive, hot-beverage,(roasted-sweet-potato,(cigarette, ((hot-pepper, bell-pepper), (eggplant, (potato, tomato)))))), (carrot, (leafy-green, (sunflower, blossom)))))))))), 
              (mushroom, ((coral, jellyfish), (((((octopus, squid), (oyster, (snail, spiral-shell))), worm), ((spider, scorpion), ((shrimp, (crab, lobster)), ((cricket, cockroach), ((honeybee, ant), (((beetle, lady-beetle), (butterfly, (mosquito, fly))))))))),
              (shark,((fish, tropical-fish, blowfish), (frog, (((turtle, (crocodile, ((sauropod, (t-rex, ((((rooster, peacock), turkey), ((goose, swan ), duck)), (flamingo, ((dove, dodo), (penguin, (owl, eagle, parrot))))))), (lizard, snake)))),
              ((kangaroo, koala), ((((elephant, mammoth)),sloth), ((hedgehog, (bat, (((((camel, two-hump-camel), llama), ((boar, pig), ((giraffe, ((deer, moose), ((ewe, goat), (water-buffalo, (bison, (ox, cow)))))), (hippopotamus, (dolphin, spouting-whale))))), ((horse, zebra), rhinoceros)),
              (((tiger, (lion, leopard)), cat), (((dog, wolf), fox), (((bear, polar-bear), panda), (seal, (skunk, (raccoon, (badger, otter)))))))))),
              ((rabbit, (chipmunk, (beaver, (hamster, (rat, mouse))))), (monkey, (orangutan, (gorilla, bust-in-silhouette)))))))))))))))));")
    } else if (emoji_type == "Apple") {
      nwk <- paste0("(microbe, (((evergreen-tree, christmas-tree),((avocado, lotus),(((tulip, ((hyacinth, (garlic, onion)), ((palm-tree, coconut), ((banana, ginger), (pineapple, (ear-of-corn, (tanabata-tree, sheaf-of-rice)))))))),((grapes, ((((peanuts, (beans, pea-pod)), (shamrock, four-leaf-clover)), (((rose, strawberry), (((red-apple, green-apple), pear), (peach, (cherry-blossom, cherries)))), (chestnut, (jack-o-lantern, (watermelon, (cucumber, melon)))))), (((hibiscus, chocolate-bar), (broccoli, leafy-green)), ((mango, ((tangerine, lemon), (maple-leaf))))))),(cactus, ((teacup-without-handle, (kiwi-fruit, blueberries)), ((olive, hot-beverage,(roasted-sweet-potato,(cigarette, ((hot-pepper, bell-pepper), (eggplant, (potato, tomato)))))), (carrot, (sunflower, blossom))))))))), 
              (mushroom, ((coral, jellyfish), (((((octopus, squid), (oyster, (snail, spiral-shell))), worm), ((spider, scorpion), ((shrimp, (crab, lobster)), ((cricket, cockroach), ((honeybee, ant), (((beetle, lady-beetle), (butterfly, (mosquito, fly))))))))),
              (shark,((fish, tropical-fish, blowfish), (frog, (((turtle, (crocodile, ((sauropod, (t-rex, ((((rooster, peacock), turkey), ((goose, swan ), duck)), (flamingo, ((dove, dodo), (penguin, (owl, eagle, parrot))))))), (lizard, snake)))),
              ((kangaroo, koala), ((((elephant, mammoth)),sloth), ((hedgehog, (bat, (((((camel, two-hump-camel), llama), ((boar, pig), ((giraffe, ((deer, moose), ((ewe, goat), (water-buffalo, (bison, (ox, cow)))))), (hippopotamus, (dolphin, spouting-whale))))), ((horse, zebra), rhinoceros)),
              (((tiger, (lion, leopard)), cat), (((dog, wolf), fox), (((bear, polar-bear), panda), (seal, (skunk, (raccoon, (badger, otter)))))))))),
              ((rabbit, (chipmunk, (beaver, (hamster, (rat, mouse))))), (monkey, (orangutan, (gorilla, bust-in-silhouette)))))))))))))))));")
    } else if (emoji_type == "Google") {
      nwk <- paste0("(microbe, ((potted-plant, ((evergreen-tree, christmas-tree),((avocado, lotus),(((tulip, ((hyacinth, (garlic, onion)), ((palm-tree, coconut), ((banana, ginger), (pineapple, (ear-of-corn, (tanabata-tree, sheaf-of-rice)))))))),((grapes, ((((peanuts, (beans, pea-pod)), (shamrock, four-leaf-clover)), (((rose, strawberry), (((red-apple, green-apple), pear), (peach, (cherry-blossom, cherries)))), (chestnut, (jack-o-lantern, (watermelon, (cucumber, melon)))))), (((hibiscus, chocolate-bar), (broccoli, leafy-green)), ((mango, ((tangerine, lemon), (maple-leaf))))))),(cactus, ((teacup-without-handle, (kiwi-fruit, blueberries)), ((olive, hot-beverage,(roasted-sweet-potato,(cigarette, ((hot-pepper, bell-pepper), (eggplant, (potato, tomato)))))), (carrot, (sunflower, blossom)))))))))), 
              (mushroom, ((coral, jellyfish), (((((octopus, squid), (oyster, (snail, spiral-shell))), worm), ((spider, scorpion), ((shrimp, (crab, lobster)), ((cricket, cockroach), ((honeybee, ant), (((beetle, lady-beetle), (butterfly, (mosquito, fly))))))))),
              (shark,((fish, tropical-fish, blowfish), (frog, (((turtle, (crocodile, ((sauropod, (t-rex, ((((rooster, peacock), turkey), ((goose, swan ), duck)), (flamingo, ((dove, dodo), (penguin, (owl, eagle, parrot))))))), (lizard, snake)))),
              ((kangaroo, koala), ((((elephant, mammoth)),sloth), ((hedgehog, (bat, (((((camel, two-hump-camel), llama), ((boar, pig), ((giraffe, ((deer, moose), ((ewe, goat), (water-buffalo, (bison, (ox, cow)))))), (hippopotamus, (dolphin, spouting-whale))))), ((horse, zebra), rhinoceros)),
              (((tiger, (lion, leopard)), cat), (((dog, wolf), fox), (((bear, polar-bear), panda), (seal, (skunk, (raccoon, (badger, otter)))))))))),
              ((rabbit, (chipmunk, (beaver, (hamster, (rat, mouse))))), (monkey, (orangutan, (gorilla, bust-in-silhouette)))))))))))))))));")
    } else if (emoji_type == "Facebook") {
      nwk <- paste0("(microbe, (((evergreen-tree, christmas-tree),((avocado, lotus),(((potted-plant, (tulip, ((hyacinth, (garlic, onion)), ((palm-tree, coconut), ((banana, ginger), (pineapple, (ear-of-corn, (tanabata-tree, sheaf-of-rice))))))))),((grapes, ((((peanuts, (beans, pea-pod)), (shamrock, four-leaf-clover)), (((rose, strawberry), (((red-apple, green-apple), pear), (peach, (cherry-blossom, cherries)))), (chestnut, (jack-o-lantern, (watermelon, (cucumber, melon)))))), (((hibiscus, chocolate-bar), (broccoli,leafy-green)), ((mango, ((tangerine, lemon), (maple-leaf))))))),(cactus, ((teacup-without-handle, (kiwi-fruit, blueberries)), ((olive, hot-beverage,(roasted-sweet-potato,(cigarette, ((hot-pepper, bell-pepper), (eggplant, (potato, tomato)))))), (carrot, (sunflower, blossom))))))))), 
              (mushroom, ((coral, jellyfish), (((((octopus, squid), (oyster, (snail, spiral-shell))), worm), ((spider, scorpion), ((shrimp, (crab, lobster)), ((cricket, cockroach), ((honeybee, ant), (((beetle, lady-beetle), (butterfly, (mosquito, fly))))))))),
              (shark,((fish, tropical-fish, blowfish), (frog, (((turtle, (crocodile, ((sauropod, (t-rex, ((((rooster, peacock), turkey), ((goose, swan ), duck)), (flamingo, ((dove, dodo), (penguin, (owl, eagle, parrot))))))), (lizard, snake)))),
              ((kangaroo, koala), ((((elephant, mammoth)),sloth), ((hedgehog, (bat, (((((camel, two-hump-camel), llama), ((boar, pig), ((giraffe, ((deer, moose), ((ewe, goat), (water-buffalo, (bison, (ox, cow)))))), (hippopotamus, (dolphin, spouting-whale))))), ((horse, zebra), rhinoceros)),
              (((tiger, (lion, leopard)), cat), (((dog, wolf), fox), (((bear, polar-bear), panda), (seal, (skunk, (raccoon, (badger, otter)))))))))),
              ((rabbit, (chipmunk, (beaver, (hamster, (rat, mouse))))), (monkey, (orangutan, (gorilla, bust-in-silhouette)))))))))))))))));")
    } else if (emoji_type == "Samsung") {
      nwk <- paste0("(microbe, (((evergreen-tree, christmas-tree),((avocado, lotus),(((potted-plant, (tulip, ((hyacinth, (garlic, onion)), ((palm-tree, coconut), ((banana, ginger), (pineapple, (ear-of-corn, (tanabata-tree, sheaf-of-rice))))))))),((grapes, ((((peanuts, (beans, pea-pod)), (shamrock, four-leaf-clover)), (((rose, strawberry), (((red-apple, green-apple), pear), (peach, (cherry-blossom, cherries)))), (chestnut, (jack-o-lantern, (watermelon, (cucumber, melon)))))), (((hibiscus, chocolate-bar), (broccoli,leafy-green)), ((mango, ((tangerine, lemon), (maple-leaf))))))),(cactus, ((teacup-without-handle, (kiwi-fruit, blueberries)), ((olive, hot-beverage,(roasted-sweet-potato,(cigarette, ((hot-pepper, bell-pepper), (eggplant, (potato, tomato)))))), (carrot, (sunflower, blossom))))))))), 
              (mushroom, ((coral, jellyfish), (((((octopus, squid), (oyster, (snail, spiral-shell))), worm), ((spider, scorpion), ((shrimp, (crab, lobster)), ((cricket, cockroach), ((honeybee, ant), (((beetle, lady-beetle), (butterfly, (mosquito, fly))))))))),
              (shark,((fish, tropical-fish, blowfish), (frog, (((turtle, (crocodile, ((sauropod, (t-rex, ((((rooster, peacock), turkey), ((goose, swan ), duck)), (flamingo, ((dove, dodo), (penguin, (owl, eagle, parrot))))))), (lizard, snake)))),
              ((kangaroo, koala), ((((elephant, mammoth)),sloth), ((hedgehog, (bat, (((((camel, two-hump-camel), llama), ((boar, pig), ((giraffe, ((deer, moose), ((ewe, goat), (water-buffalo, (bison, (ox, cow)))))), (hippopotamus, (dolphin, spouting-whale))))), ((horse, zebra), rhinoceros)),
              (((tiger, (lion, leopard)), cat), (((dog, wolf), fox), (((bear, polar-bear), panda), (seal, (skunk, (raccoon, (badger, otter)))))))))),
              ((rabbit, (chipmunk, (beaver, (hamster, (rat, mouse))))), (monkey, (orangutan, (gorilla, bust-in-silhouette)))))))))))))))));")
    }
    return(nwk)
  }
  
  # Get Newick tree string based on emoji type
  nwk <- generate_tree(emoji_type)
  tree = read.tree(text = nwk)
  p <- ggtree(tree, layout = "circular") +
    geom_tiplab(aes(image=paste0(imgdir, '/', label, '.png')), geom="image", offset=3, align = T, size=.018)
  
}

#save as png
png(filename = file.path(imgdir, paste0("emojitree_",emoji_type,"_", treetype,".png")), width = 10, height = 10, unit = "cm", res = 1000)
p
dev.off()  

