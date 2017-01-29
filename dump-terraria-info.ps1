$TerrariaPath = "C:\Program Files (x86)\Steam\SteamApps\common\Terraria\"
function Get-LispKeyword {
    Param(
        [parameter(ValueFromPipeline)][string]$OldName
    )
    Process {
        # Make the first character lowercase
        $lowerThenUpper = ($OldName[0].ToString().ToLower())+$OldName.Substring(1)
        # At each capital letter, replace with dash and lowercase letter
        $lower_lisp_name = [char[]]$lowerThenUpper | ForEach-Object {
            $c = $_.ToString()
            If ($c -ceq $c.ToUpper()) {
                "-"+$c.ToLower()
            } Else {
                $c
            }
        }
        # Capitalize the string and prefix a colon
        $lisp_keyword = ":"+($lower_lisp_name -join "").ToUpper()
        # Apply some corner-case replacements
        $lisp_keyword -replace 'N-P-C-S','NPCS' `
                      -replace 'N-P-C','NPC'
    }
}
function New-LispCons {
    Param(
        [parameter(Mandatory)][string]$car,
        [parameter(Mandatory)][string]$cdr
    )
    "({0} . {1})" -f $car,$cdr
}
function New-LispList {
    Param(
        [parameter(ValueFromPipeline)][string[]]$Elements
    )
    "("+($Elements -join " ")+")"
}
function New-LispAList {
    Param(
        [parameter(Mandatory)][hashtable]$Hash
    )
    New-LispList -Elements ($Hash.GetEnumerator() | ForEach-Object {
        New-LispCons -car $_.Key -cdr $_.Value
    })
}
function Get-LispColor {
    Param(
        [parameter(ValueFromPipeline,Mandatory)]$Color
    )
    Process {
        New-LispList -Elements @($Color.R, $Color.G, $Color.B, $Color.A)
    }
}


### Load Terraria
[Reflection.Assembly]::LoadFile($TerrariaPath+"\Terraria.exe") >$null
[Reflection.Assembly]::LoadWithPartialName("Microsoft.Xna.Framework") >$null

### Tile ID and category stuff
$Tiles = @("") * [Terraria.ID.TileID]::Count
[Terraria.ID.TileID].GetFields() | Where-Object {$_.Name -ne "Count"} | ForEach-Object {
    $Tiles[$_.GetValue($null)] = New-Object -TypeName PSObject -Property ([Ordered] @{"Name"=$_.Name;"Sets"=@();"Colors"=@()})
}
@([Terraria.ID.TileID+Sets].GetFields() +
  [Terraria.ID.TileID+Sets+Conversion].GetFields()) | Where-Object {$_.FieldType -eq [Boolean[]]} `
  | Sort-Object -Property Name | ForEach-Object {
    $name = $_.Name
    $_.GetValue($null) | ForEach-Object -Begin { $i = 0 } -Process {
        If ($_) {
            $Tiles[$i].Sets += $name
        }
        $i++
    }
}
@([Terraria.ID.TileID+Sets+RoomNeeds].GetFields()) | Where-Object {$_.FieldType -eq [Int32[]]} `
  | Sort-Object -Property Name | ForEach-Object {
    $name = $_.Name
    $_.GetValue($null) | ForEach-Object {
        $Tiles[$_].Sets += $name
    }
}

### Wall ID and category stuff
$Walls = @("") * [Terraria.ID.WallID]::Count
[Terraria.ID.WallID].GetFields() | Where-Object {$_.Name -ne "Count"} | ForEach-Object {
    $Walls[$_.GetValue($null)] = New-Object -TypeName PSObject -Property ([Ordered] @{"Name"=$_.Name;"Sets"=@();"Colors"=@()})
}
@([Terraria.ID.WallID+Sets].GetFields() +
  [Terraria.ID.WallID+Sets+Conversion].GetFields()) | Where-Object {$_.FieldType -eq [Boolean[]]} `
  | Sort-Object -Property Name | ForEach-Object {
    $name = $_.Name
    $_.GetValue($null) | ForEach-Object -Begin { $i = 0 } -Process {
        If ($_) {
            $Walls[$i].Sets += $name
        }
        $i++
    }
}

### Map color stuff
## Setup
[Terraria.Map.MapHelper]::Initialize()
$Terraria_Map_MapHelper_ = [Terraria.Map.MapHelper].GetFields([Reflection.BindingFlags]"NonPublic,Static")
$_colors = ($Terraria_Map_MapHelper_ | Where-Object {$_.Name -eq "colorLookup"}).GetValue($null)
## Empty
$EmptyColor = $_colors[0]
## Tiles
@([Terraria.Map.MapHelper]::tileLookup | ForEach-Object -Begin {$id = 0} -Process {
    $base = $_
    $count = [Terraria.Map.MapHelper]::tileOptionCounts[$id]
    If ($count -gt 0) {
        $Tiles[$id].Colors = [array]($_colors[$base..($base+$count-1)])
    }
    $id++
})
## Walls
@([Terraria.Map.MapHelper]::wallLookup | ForEach-Object -Begin {$id = 0} -Process {
    $base = $_
    $count = [Terraria.Map.MapHelper]::wallOptionCounts[$id]
    If ($count -gt 0) {
        $Walls[$id].Colors = [array]($_colors[$base..($base+$count-1)])
    }
    $id++
})
## Others
$_liquidPos = ($Terraria_Map_MapHelper_ | Where-Object {$_.Name -eq "liquidPosition"}).GetValue($null)
$_skyPos = ($Terraria_Map_MapHelper_ | Where-Object {$_.Name -eq "skyPosition"}).GetValue($null)
$_dirtPos = ($Terraria_Map_MapHelper_ | Where-Object {$_.Name -eq "dirtPosition"}).GetValue($null)
$_rockPos = ($Terraria_Map_MapHelper_ | Where-Object {$_.Name -eq "rockPosition"}).GetValue($null)
$_hellPos = ($Terraria_Map_MapHelper_ | Where-Object {$_.Name -eq "hellPosition"}).GetValue($null)
$LiquidColors = $_colors[$_liquidPos..($_skyPos-1)]
$SkyColors = $_colors[$_skyPos..($_dirtPos-1)]
$DirtColors = $_colors[$_dirtPos..($_rockPos-1)]
$RockColors = $_colors[$_rockPos..($_hellPos-1)]
$HellColor = $_colors[$_hellPos]

### Output
New-LispAList -Hash @{
    ":EMPTY" = $EmptyColor | Get-LispColor;
    ":TILES" = (,@($Tiles | ForEach-Object {
        New-LispList -Elements @(
            (Get-LispKeyword -OldName $_.Name),
            (New-LispList -Elements ([array]($_.Sets | Get-LispKeyword))),
            (,@($_.Colors | Get-LispColor) | New-LispList)
        )
    }) | New-LispList);
    ":WALLS" = (,@($Walls | ForEach-Object {
        New-LispList -Elements @(
            (Get-LispKeyword -OldName $_.Name),
            (New-LispList -Elements ([array]($_.Sets | Get-LispKeyword))),
            (,@($_.Colors | Get-LispColor) | New-LispList)
        )
    }) | New-LispList);
    ":LIQUIDS" = (,@($LiquidColors | Get-LispColor) | New-LispList);
    ":SKY" = (,@($SkyColors | Get-LispColor) | New-LispList);
    ":DIRT" = (,@($DirtColors | Get-LispColor) | New-LispList);
    ":ROCK" = (,@($RockColors | Get-LispColor) | New-LispList);
    ":HELL" = $HellColor | Get-LispColor
}
