#lang racket
(require "../structs.rkt")
(provide (all-defined-out))

;; Heroes' rarities of Spirits
(define spirits-rarities
  (list
   (rarity "5-Stars Lord Hero"   #e0.0004 5 #t
           '(
             "Torodor"        "King Harz"      "Aracha Morrigan" "Twinfiend"     
             "Venoma"         "Ajax"           "Laya"
             ))
   (rarity "5-Stars Normal Hero" #e0.0046 5 #f
           '(
             "Zelus"          "Valkyra"        "Calypso"         "Nocturne"     
             "Setram"         "Khamet"         "Valeriya"        "Brokkir"      
             "Regulus"        "Nyx"            "Shamir"          "Abomination"  
             "Cerberus"       "Aylin"          "Magmus"          "Apsan"        
             "Razaak"         "Laseer"         "Ares"            "Crach"        
             "Sadie"          "SIE"            "Ezryn"           "Azhor"        
             "Alaura"         "Kria"           "Kineza"          "Kai"          
             "Orim"           "Anai"           "Ferssi"          "Artemis"      
             "Trusk"          "Lucius"         "Arrogance"       "Zilitu"       
             "Salazar"        "Elowyn"         "Hatssut"         "Vierna"       
             "Hex"            "Magda"          "Constance"       "Boreas"       
             "Numera"         "Xaris"          "Glacius"         "Talula"       
             "Beelzebub"      "Serephina"      "Xena"            "Twyla"        
             "Beatrix"        "Edith"          "Lord Phineas"    "Lyra"         
             "Aeris"          "Krodor"         "Helga"           "Yuri"         
             "Lynx"
             ))
   (rarity "4-Stars Lord Hero"   #e0.0012 4 #t
           '(
             "Wrath"          "Ain"            "Isolde"          "Luneria"      
             "Aeon"           "Raiden"         "Pyros"
             ))
   (rarity "4-Stars Normal Hero" #e0.0788 4 #f
           '(
             "Komodo"         "Gluttony"       "Cyrene"          "Baron"        
             "Scorch"         "Daemon"         "Titus"           "Livian"       
             "Olague"         "Dalyn"          "Meriel"          "Cyclone"      
             "Voroth"         "Deimos"         "Aveline"         "Estrid"       
             "Ardeth"         "Janghar"        "Atrox"           "Jorge"        
             "Drayga"         "Rhox"           "Brienne"         "Maul"         
             "Brunor"         "Theowin"        "Idril"           "Tazira"       
             "Harpun"         "Esme"           "Marri"           "Soleil"       
             "Nauvras"        "Nazeem"         "Greed"           "Eona"         
             "Osiren"         "Azzoth"         "Ai"              "Imani"        
             "Selkath"        "Laurel"         "Nisalt"          "Kalina"       
             "Dolores"        "Vortex"         "Lightlocke"      "Midan"        
             "Hollow"         "Nissandei"      "Lili"
             ))
   (rarity "3-Stars Lord Hero"   #e0.0399 3 #t
           '(
             "Borut"          "Dagna"          "Niro"
             ))
   (rarity "3-Stars Normal Hero" #e0.3751 3 #f
           '(
             "Rex"            "Glen"           "Narvi"           "Skulf"        
             "Gnash"          "Decimus"        "Barclay"         "Ogrul"        
             "Rhutu"          "Ghorza"         "Shelor"          "Duradel"      
             "Morene"         "Amahle"         "Cuke"            "Drogo"        
             "Elukas"         "Voltus"         "Sorzus"          "Gonkba"       
             "Nunea"          "Camille"        "Aryn"            "Spring"       
             "Autumn"
             ))
   (rarity "2-Stars Normal Hero"        #e0.5    2 #f
           '(
             "Hayden"         "Rogers"         "Arlow"           "Cutter"       
             "Liam"           "Rum-Nose"       "Wagrak"          "Ryder"        
             "Preter"         "Halder"         "Jonas"           "Skreef"       
             "Langlyn"
             ))))

;; Heroes' rarities of Divine
(define divine-rarities
  (list
   (rarity "5-Stars Lord Hero"   #e0.004 5 #t
           '(
             "Torodor"        "King Harz"      "Aracha Morrigan" "Twinfiend"     
             "Venoma"         "Ajax"           "Laya"
             ))
   (rarity "5-Stars Normal Hero" #e0.056 5 #f
           '(
             "Zelus"          "Valkyra"        "Calypso"         "Nocturne"     
             "Setram"         "Khamet"         "Valeriya"        "Brokkir"      
             "Regulus"        "Nyx"            "Shamir"          "Abomination"  
             "Cerberus"       "Aylin"          "Magmus"          "Apsan"        
             "Razaak"         "Laseer"         "Ares"            "Crach"        
             "Sadie"          "SIE"            "Ezryn"           "Azhor"        
             "Alaura"         "Kria"           "Kineza"          "Kai"          
             "Orim"           "Anai"           "Ferssi"          "Artemis"      
             "Trusk"          "Lucius"         "Arrogance"       "Zilitu"       
             "Salazar"        "Elowyn"         "Hatssut"         "Vierna"       
             "Hex"            "Magda"          "Constance"       "Boreas"       
             "Numera"         "Xaris"          "Glacius"         "Talula"       
             "Beelzebub"      "Serephina"      "Xena"            "Twyla"        
             "Beatrix"        "Edith"          "Lord Phineas"    "Lyra"         
             "Aeris"          "Krodor"         "Helga"           "Yuri"         
             "Lynx"
             ))
   (rarity "4-Stars Lord Hero"   #e0.0122 4 #t
           '(
             "Wrath"          "Ain"            "Isolde"          "Luneria"      
             "Aeon"           "Raiden"         "Pyros"
             ))
   (rarity "4-Stars Normal Hero" #e0.9278 4 #f
           '(
             "Komodo"         "Gluttony"       "Cyrene"          "Baron"        
             "Scorch"         "Daemon"         "Titus"           "Livian"       
             "Olague"         "Dalyn"          "Meriel"          "Cyclone"      
             "Voroth"         "Deimos"         "Aveline"         "Estrid"       
             "Ardeth"         "Janghar"        "Atrox"           "Jorge"        
             "Drayga"         "Rhox"           "Brienne"         "Maul"         
             "Brunor"         "Theowin"        "Idril"           "Tazira"       
             "Harpun"         "Esme"           "Marri"           "Soleil"       
             "Nauvras"        "Nazeem"         "Greed"           "Eona"         
             "Osiren"         "Azzoth"         "Ai"              "Imani"        
             "Selkath"        "Laurel"         "Nisalt"          "Kalina"       
             "Dolores"        "Vortex"         "Lightlocke"      "Midan"        
             "Hollow"         "Nissandei"      "Lili"
             ))))

;; Heroes' rarities of Ancient
(define ancient-rarities
  (list
   (rarity "5-Stars Lord Hero" #e0.008 5 #t
           '(
             "Torodor"        "King Harz"      "Aracha"          "Morrigan"     
             "Twinfiend"      "Venoma"         "Ajax"            "Ghan"         
             "Elddr"          "Laya"           "Cyrus"           "Demi"         
             "Solcadens"      "Praetus"        "Valderon"        "Iovar"        
             "Ymiret"         "Ingrid"         "Rygar"
             ))
   (rarity "5-Stars Normal Hero" #e0.01 5 #f
           '(
             "Lugaru"         "Dassomi"        "Valeriya"        "Cerberus"     
             "Kaede"          "Durza"          "Sargak"          "Carnelian"    
             "Ardea"          "Gisele"         "Thallen"         "Pelagios"     
             "Eunomia"        "Valkyra"        "Constance"       "Uredin"       
             "Admiral Claw"   "Alistair"       "Jeera"           "Lu Bu"
             ))
   (rarity "4-Stars Lord Hero" #e0.06 4 #t
           '(
             "Wrath"          "Ain"            "Isolde"          "Luneria"      
             "Aeon"           "Raiden"         "Pyros"           "Vladov"
             "Elysia"
             ))
   (rarity "4-Stars Normal Hero" #e0.05 4 #f
           '(
             "Brienne"        "Soleil"         "Cyrene"          "Nauvras"      
             "Brunor"         "Lightlocke"     "Livian"          "Greed"        
             "Aveline"        "Idril"          "Eona"            "Atrox"        
             "Vargus"         "Jorge"
             ))
   (rarity "3-Stars Lord Hero" #e0.27 3 #t
           '(
             "Borut"          "Dagna"          "Niro"
             ))
   (rarity "3-Stars Normal Hero" #e0.602 3 #f
           '(
             "Morene"         "Amahle"         "Cuke"            "Drogo"        
             "Elukas"         "Voltus"         "Sorzus"          "Gonkba"       
             "Rex"            "Glen"           "Narvi"           "Skulf"        
             "Gnash"          "Decimus"        "Barclay"         "Ogrul"        
             "Rhutu"          "Ghorza"         "Shelor"          "Duradel"      
             "Nunea"          "Camille"        "Aryn"            "Spring"       
             "Autumn"
             ))
   ))


