library(tidyverse)
library(forcats)

panel <- mutate(panel, 
                sexn = factor(sexn, levels = 1:2, labels = c("Homme", "Femme")),
                dipp = factor(dipp, levels = 1:4, labels = c("Aucun diplome ou CEP", "Brevet ou CAP", "Bac ou equivalent", "Etudes superieures")))


# PC08
# Q51, équipement:
# 1 / POSTE DE RADIO, TRANSISTOR
# 2 / TELEVISEUR (dont téléviseur 16/9)
# 3 / HOME CINEMA
# 4 / MAGNETOSCOPE ou LECTEUR DVD de salon
# 5 / Graveur/enregistreur DVD relié à la télévision
# 6 / LECTEUR DVD PORTABLE (lecteurs DVD avec écran intégré) OU BA LADEUR MULTIMEDIA permettant de regarder des vidéos (exemple  Ipod video, Archos vidéo)
# 7 / BALADEUR MP3 sans tenir compte de la fonction MP3 d'un télép hone portable
#       8 / BALADEUR CD (avec radio, cassette,...)
#       9 / Platine ou électrophone pour écouter des disques vinyles
#      10 / LECTEUR de CD
#      11 / CONSOLE DE JEUX FIXE qui se branchent sur un téléviseur, c'e st-à-dire une console de jeux sans écran intégré
# 12 / CONSOLE DE JEUX PORTABLE, c'est-à-dire avec écran intégré
#      13 / ASSISTANT PERSONNEL (PDA) de type Palm Pilot, sans tenir com pte de la fonction PDA d'un téléphone portable
# 14 / APPAREIL PHOTO NUMERIQUE, c'est-à-dire un appareil photo ave c écran intégré qui n'ont pas besoin de pellicules, sans ten ir compte de la fonction appareil photo d'un téléphone porta ble
#      15 / CAMERA NUMERIQUE ou CAMESCOPE NUMERIQUE
#      16 / MICRO ORDINATEUR FIXE (de bureau), appartenant à votre foyer
#      17 / MICRO ORDINATEUR PORTABLE, appartenant à votre foyer ou étan t mis à disposition par un employeur
#      18 / TELEPHONE PORTABLE, y compris ceux mis à disposition par un employeur
# L = 1 - 3
#       1 / OUI
#       2 / NON
#       3 / [nsp]
# 
# 
# V = Q64 S(1-2) "J12.- Au cours des 12 derniers mois, avez-vous joué, chez vo us ou ailleurs, à des jeux vidéos, que ce soit sur une conso le de jeux, un téléphone portable, un écran de TV ou un ordi nateur ? "
# F = Saisie(136)
# L = 1 - 2
#       1 / oui
#       2 / non
# 
# V = Q65 S(1-5) "J13.- En général, à quelle fréquence jouez-vous à des jeux v idéos ? ENQ : MONTRER LISTE 9 "
# F = Saisie(137)
# L = 1 - 5
#       1 / tous les jours ou presque
#       2 / 3 ou 4 jours par semaine
#       3 / environ 1 ou 2 jours par semaine
#       4 / environ 1 à 3 jours par mois
#       5 / plus rarement
# 
# V = J14BC M(1-14) "J14.- Sur quel appareil jouez-vous le plus souvent ? ENQUETEUR : une seule réponse On parle des appareils appartenant au foyer"
# F = Saisie(138-151 / parRang)
# L = 1 - 14
#       1 / sur une console de jeux (du foyer)
#       2 / sur un téléphone portable (du foyer)
#       3 / sur un écran de TV (du foyer)
#       4 / sur un ordinateur (du foyer)
#       5 / sur un autre appareil appartenant au foyer
#       6 / Sous total appareil appartenant au foyer
#       7 / Sur une console de jeux (Playstation, PS2, XBOX,...) (hors foyer)
#       8 / Sur un téléphone portable (hors foyer)
#       9 / Sur un écran de TV (hors foyer)
#      10 / Sur un ordinateur (hors foyer)
#      11 / Sur la console de jeux Wii (hors foyer)
#      12 / Sur une console portable (Nintendo DS, PSP) (hors foyer)
#      13 / Autre (hors foyer)
#      14 / Sous total appareil hors foyer
#     Rebut / [Ne joue pas à des jeux vidéos]
# 
# V = J15C M(1-99) "J15.- Au cours des 12 derniers mois, avez-vous joué à... ENQUETEUR : MONTRER LISTE 10 - on parle toujours des jeux vi déos"
# F = Saisie(452-550 / parRang)
# L = 1 - 99
#       1 / des jeux d'action dont jeux FPS, de combat ou de plateforme (Super Mario,...)
# 2 / des jeux de sport (Pro Evolution Soccer, Gran Turismo, Tenni s,...)
# 3 / des jeux de patience, de rôle ou de stratégie
# 4 / des jeux de société (cartes, échecs,...)
# 5 / des jeux de hasard (casino, loterie,...)
# 6 / des jeux éducatifs (adibou,...)
# 8 / des jeux de simulations (dont aviation...)
# 9 / MMORPG
# 98 / Autres
# 99 / NSP
# Rebut / [Ne joue pas à des jeux vidéos]
# Total / Total
# 
# V = TPJVH Q "Durée hebdomadaire en heures devant Jeux Vidéo"
# F = Saisie(813-815)
# 
# V = RJV S(1-6) "Durée de jeux vidéo par semaine"
# F = Saisie(816)
# L = 1 - 6
# 1 / Moins d'une heure
#       2 / de 1 à moins de 3 heures
#       3 / de 3 à moins de 5 heures
#       4 / de 5 à moins de 10 heures
#       5 / 10 heures et plus
#       6 / NSP
#     Rebut / [Ne joue pas à des jeux vidéos ou y joue sur ordinateur]
# V = Q89 M(1-23) "O25.- Parmi les usages suivants d'internet, quels sont ceux que vous avez fait ? ENQUETEUR : MONTRER LISTE 15 - PLUSIEURS REPONSES POSSIBLES "
# F = Saisie(1345-1367 / parRang)
# L = 1 - 23
#       1 / Envoyer/recevoir des mails
#       2 / "Communiquer à l'aide d'une messagerie instantanée "
#       3 / Faire des recherches documentaires, consulter des bases de d onnées
#       4 / Regarder en direct un programme de télévision
#       5 / Ecouter en direct la radio
#       6 / Lire en ligne des journaux ou magazines
#       7 / Visiter des blogs ou des sites personnels
#       8 / Chercher des informations pratiques
#       9 / Gérer vos affaires personnelles (banque, impôts, payer des f actures.)
#      10 / Suivre un enseignement en ligne, une formation
#      11 / Vendre ou acheter aux enchères (ebay,...)
#      12 / Acheter ou commander en ligne
#      13 / Télécharger des logiciels, des programmes, de la musique ou autre chose
#      14 / Aller sur un site de partage de fichiers (Dailymotion, Youtu be, Emule,.)
#      15 / Jouer à des jeux en réseau (dont jeux d'argent)
#      16 / Participer à des chats ou des forums de discussion, écrire d es commentaires
#      17 / Aller sur un site de rencontre (Meetic,...)
#      18 / Créer ou mettre à jour votre profil sur un site comme My Spa ce, Facebook, etc..
#      19 / Créer ou mettre à jour un blog ou un site personnel
#      20 / Mettre des photos, vidéos ou de la musique en ligne
#      21 / Visiter un musée ou une exposition en ligne
#      22 / [autre]
#      23 / [aucun]
#     Rebut / [N'utilise pas d'ordinateur, pas internet]
# 
# V = Q90[5] L(51) "O26.- PRECISER AUTRE "
# F = Saisie(1368-1418 / Pas=51)
# O =
#       1 /
#       2 /
#       3 /
#       4 /
#       5 /
# 
# V = Q91 S(1-2) "O26a.- Quand vous lisez des journaux ou magazines en ligne, s'agit-il de journaux ou de magazines que vous lisez réguliè rement sous forme papier ? "
# F = Saisie(1623)
# L = 1 - 2
#       1 / oui
#       2 / non
#     Rebut / [Ne se connecte pas à internet ou ne lit pas de journaux sur internet]
# 
# V = Q92[8] S(1-2) "O26b.- Avez-vous déjà téléchargé %S ? "
# F = Saisie(1624 / Pas=1)
# O =
#       1 / des logiciels, programmes
#       2 / des jeux vidéo
#       3 / de la musique
#       4 / des films, séries, vidéos
#       5 / des émissions télé
#       6 / des émissions de radio
#       7 / des photos, images
#       8 / des documents écrits (articles, livres.)
# L = 1 - 2
#       1 / oui
#       2 / non
# 
# 
# V = EM56C S(1-98) "EM56.- Quand vous écoutez à votre domicile de la musique san s rien faire d'autre, est-ce le plus souvent à partir de... ENQUETEUR : MONTRER LISTE 27"
# F = Saisie(5594-5595)
# L = 1 - 98
#       1 / ...de CD ou cassettes audio
#       2 / ...de DVD musicaux
#       3 / ...de la radio
#       4 / ...de la télévision (chaines musicales comme MTV,...)
#       5 / ...d'internet
#       6 / ...d'un ordinateur (musique enregistrée sur le disque dur)
#       8 / Lecteur MP3
#       9 / disques vinyles
#      10 / Téléphone portable
#      98 / Autre
#     Rebut / [N'écoute pas de musique ou écoute de la musique en faisant autre chose]
