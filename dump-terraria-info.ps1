$TerrariaPath = "C:\Program Files (x86)\Steam\SteamApps\common\Terraria"

# Load Terraria
[Reflection.Assembly]::LoadFile($TerrariaPath+"\Terraria.exe") > $null
# Load XNA
[Reflection.Assembly]::LoadWithPartialName("Microsoft.Xna.Framework") > $null
### Tile ID stuff
$TileIDList = @{}
[Terraria.ID.TileID].GetFields() | Where-Object {$_.Name -ne "Count"} | ForEach-Object {
    $TileIDList.Add($_.Name,$_.GetValue($null))
}
### Color stuff
# Populate color information for minimap
[Terraria.Map.MapHelper]::Initialize()
# Get the main color list
$PrivateFields = [Terraria.Map.MapHelper].GetFields([Reflection.BindingFlags]"NonPublic,Static")
$ColorLookup = ($PrivateFields | Where-Object {$_.Name -eq "colorLookup"}).GetValue($null)
# Get the various positions
$TilePosition = ($PrivateFields | Where-Object {$_.Name -eq "tilePosition"}).GetValue($null)
$WallPosition = ($PrivateFields | Where-Object {$_.Name -eq "wallPosition"}).GetValue($null)
$LiquidPosition = ($PrivateFields | Where-Object {$_.Name -eq "liquidPosition"}).GetValue($null)
$SkyPosition = ($PrivateFields | Where-Object {$_.Name -eq "skyPosition"}).GetValue($null)
$DirtPosition = ($PrivateFields | Where-Object {$_.Name -eq "dirtPosition"}).GetValue($null)
$RockPosition = ($PrivateFields | Where-Object {$_.Name -eq "rockPosition"}).GetValue($null)
$HellPosition = ($PrivateFields | Where-Object {$_.Name -eq "hellPosition"}).GetValue($null)
# Get the variation counts for tiles
$TileOptionCounts = [Terraria.Map.MapHelper]::tileOptionCounts

<# Make the sexp for terraria-map-dump
 (TILE-NAME-LIST POSITION-LIST COLOR-LIST)
 TILE-NAME-LIST := NIL | (KEYWORD . TILE-NAME-LIST)
 POSITION-LIST := NIL | (INTEGER . POSITION-LIST)
 COLOR-LIST := NIL | (COLOR . COLOR-LIST)
 COLOR := (INTEGER INTEGER INTEGER INTEGER)
#>
function Get-LispName {
    Param(
        [parameter(ValueFromPipeline)][string]$oldName
    )
    # Ensure the first character is lowercase
    $lowerThenUpperName=($oldName[0].ToString().ToLower())+$oldName.Substring(1)
    # At each capital letter, replace it with a dash then lowercase letter
    $lower_lisp_name=[char[]]$lowerThenUpperName | ForEach-Object {
        $input = $_.ToString()
        If ($input -ceq $input.ToUpper()) {
            "-"+$input.ToLower()
        } Else { $input.ToString() }}
    # Make the name capital
    ($lower_lisp_name -join "").ToUpper()
}
$tileListSexp = $TileIDList.GetEnumerator() | Sort-Object Value | ForEach-Object `
-begin {
    $sexp = "("
} -process {
    $sexp += ":"+($_.Name | Get-LispName)+" "
} -end {
    $sexp.TrimEnd()+")"
}
$positionListSexp = "({0} {1} {2} {3} {4} {5} {6})" -f $TilePosition, $WallPosition, $LiquidPosition, $SkyPosition, $DirtPosition, $RockPosition, $HellPosition
$colorListSexp = $ColorLookup | ForEach-Object `
-begin {
    $sexp = "("
} -process {
    $sexp += "({0} {1} {2} {3})" -f $_.R,$_.G,$_.B,$_.A
} -end {
    $sexp+")"
}
$finalSexp = "({0}{1}{2})" -f $tileListSexp, $positionListSexp, $colorListSexp