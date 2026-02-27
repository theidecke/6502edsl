-- | VIC-II graphics chip registers ($D000-$D02E).
module Target.C64.Mem.VIC where

import Data.Word (Word16)

-- Sprite positions
vicSprite0X, vicSprite0Y :: Word16
vicSprite0X = 0xD000
vicSprite0Y = 0xD001

vicSprite1X, vicSprite1Y :: Word16
vicSprite1X = 0xD002
vicSprite1Y = 0xD003

vicSprite2X, vicSprite2Y :: Word16
vicSprite2X = 0xD004
vicSprite2Y = 0xD005

vicSprite3X, vicSprite3Y :: Word16
vicSprite3X = 0xD006
vicSprite3Y = 0xD007

vicSprite4X, vicSprite4Y :: Word16
vicSprite4X = 0xD008
vicSprite4Y = 0xD009

vicSprite5X, vicSprite5Y :: Word16
vicSprite5X = 0xD00A
vicSprite5Y = 0xD00B

vicSprite6X, vicSprite6Y :: Word16
vicSprite6X = 0xD00C
vicSprite6Y = 0xD00D

vicSprite7X, vicSprite7Y :: Word16
vicSprite7X = 0xD00E
vicSprite7Y = 0xD00F

-- Sprite X position MSB (bit i = sprite i X bit 8)
vicSpriteXMSB :: Word16
vicSpriteXMSB = 0xD010

-- Screen control
vicControlY :: Word16
vicControlY = 0xD011

vicRasterLine :: Word16
vicRasterLine = 0xD012

vicLightPenX :: Word16
vicLightPenX = 0xD013

vicLightPenY :: Word16
vicLightPenY = 0xD014

vicSpriteEnable :: Word16
vicSpriteEnable = 0xD015

vicControlX :: Word16
vicControlX = 0xD016

vicSpriteExpandY :: Word16
vicSpriteExpandY = 0xD017

vicMemorySetup :: Word16
vicMemorySetup = 0xD018

vicInterruptStatus :: Word16
vicInterruptStatus = 0xD019

vicInterruptEnable :: Word16
vicInterruptEnable = 0xD01A

-- Sprite properties
vicSpritePriority :: Word16
vicSpritePriority = 0xD01B

vicSpriteMulticolor :: Word16
vicSpriteMulticolor = 0xD01C

vicSpriteExpandX :: Word16
vicSpriteExpandX = 0xD01D

vicSpriteSpriteCollision :: Word16
vicSpriteSpriteCollision = 0xD01E

vicSpriteBackgroundCollision :: Word16
vicSpriteBackgroundCollision = 0xD01F

-- Colors
vicBorderColor :: Word16
vicBorderColor = 0xD020

vicBackgroundColor0 :: Word16
vicBackgroundColor0 = 0xD021

vicBackgroundColor1 :: Word16
vicBackgroundColor1 = 0xD022

vicBackgroundColor2 :: Word16
vicBackgroundColor2 = 0xD023

vicBackgroundColor3 :: Word16
vicBackgroundColor3 = 0xD024

vicSpriteMulticolor0 :: Word16
vicSpriteMulticolor0 = 0xD025

vicSpriteMulticolor1 :: Word16
vicSpriteMulticolor1 = 0xD026

vicSprite0Color :: Word16
vicSprite0Color = 0xD027

vicSprite1Color :: Word16
vicSprite1Color = 0xD028

vicSprite2Color :: Word16
vicSprite2Color = 0xD029

vicSprite3Color :: Word16
vicSprite3Color = 0xD02A

vicSprite4Color :: Word16
vicSprite4Color = 0xD02B

vicSprite5Color :: Word16
vicSprite5Color = 0xD02C

vicSprite6Color :: Word16
vicSprite6Color = 0xD02D

vicSprite7Color :: Word16
vicSprite7Color = 0xD02E
