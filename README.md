Pour que tout fonctione, il faut:

1. ajouter à la racine du répertoire un fichier `credentials.R` contenant votre login et mot de passe personnel sur le nextcloud envoyé par Ph. L.:

~~~r
Sys.setenv(NEXTCLOUD_USER = "LOGIN", 
           NEXTCLOUD_PASSWORD = "PASSWORD",
           NEXTCLOUD_URL = "https://nextcloud.neomad.org")
~~~

2. ajouter un répertoire `01_data` contenant les différentes données et dictionnaires de codes envoyés jusque là :

    - `Dictionnaire des codes EPC 2018 (22 janvier 2021).xlsx`
    - `Dictionnaire des codes pseudo panel EPC 1973-2018 (22 janvier 2021).xlsx`
    - `panel7318.sas7bdat`

Les scripts présents dans `02_import` permettent de produire les bases de données et de les recoder. Les scripts `import_\*.R` doivent être executés une fois, puis à chaque fois que la base change. L'import des données 2018 va directement chercher la base la plus à jour sur le site du ministère.

Ensuite, les fichiers `\*.Rmd` présents dans `04_analyses` contiennent le code pour produire les analyses brutes.

