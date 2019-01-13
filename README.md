# CleanMagic for Hakyll

This theme is a fork of [CleanMagicMedium-Jekyll](https://github.com/SpaceG/CleanMagicMedium-Jekyll) originally published by Lucas Gatsas.

## Installation instructions

Note that this theme has specific design features that require a custom written [siteCtx context](https://github.com/katychuang/CleanMagic-hakyll/blob/master/site.hs#L68) that contains specific field values mapped to the template fields. For example, `$site_description$` in the template files is mapped to the constant field with the string, "my beautiful blog".

```haskell
siteCtx :: Context String
siteCtx = 
    constField "baseurl" "http://localhost:8000" `mappend` 
    constField "site_description" "my beautiful blog" `mappend`
    constField "instagram_username" "katychuang.nyc" `mappend`
    constField "twitter_username" "katychuang" `mappend`
    constField "github_username" "katychuang" `mappend`
    constField "google_username" "katychuang" `mappend`
    defaultContext
```

Once you have the field strings written as how you'd like it, make sure that you're connecting this function where your pages are being rendered, for example in creating the `index.html` page, you want to include it, similar to the following ([ref](https://github.com/katychuang/CleanMagic-hakyll/blob/master/site.hs#L54)): 

```haskell
match "index.html" $ do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
                listField "posts" postCtx (return posts) `mappend`
                constField "title" "Home"                `mappend`
                siteCtx -- INCLUDE THE CUSTOM FUNCTION, it sends variable/values to the template in the lines below

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls
```

## Building with Stack

Building your site using stack is covered [here](https://jaspervdj.be/hakyll/tutorials/02-basics.html).

A quick recap:

```
$ stack build
$ stack exec CleanMagic-hakyll build # or rebuild if you made changes to site.hs
```

Then
```
$ stack exec CleanMagic-hakyll watch
```

And access the site at:

http://127.0.0.1:8000

--

This theme features a top navigation bar, and an area for beautiful header background images. Clean and serene, it's sure to give your posts an extra polish.

![Preview](https://github.com/katychuang/hakyll-cssgarden/blob/master/gallery/images/cleanMagic_hakyll-index.png?raw=true)

![Preview](https://raw.githubusercontent.com/SpaceG/spaceg.github.io/5f240c5e8b3f8e2cb9f776688466de651d5d8958/img/intro-theme-1.png)
