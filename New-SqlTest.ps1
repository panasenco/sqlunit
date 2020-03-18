<#
.Synopsis
    Generates SQL test script for a view from /* [UNIT] ... */ comments in the view's definition using sqlunit.
.Description
    Parses view definition files for /* [UNIT] ... */ comments, extracts the contents of those comments, and passes
    them to [sqlunit](https://github.com/panasenco/sqlunit). See the sqlunit README for syntax and optimizations.
.Parameter Path
    File to generate unit tests for. If you want to generate unit tests for multiple files, you have to pass them
    through the pipeline.
.Parameter Prefix
    Use <prefix>.<file basename> as the table to pass to sqlunit to select from.
    If this parameter is a list, it is assumed to be of the form <mainprefix>,<secondaryprefix>, where mainprefix
    is applied to the file named exactly like its parent directory, and secondaryprefix is applied to all other files.
.Parameter Suffix
    Append provided suffix to all table names
.Example
    Get-ChildItem .\Research\EpicOnCoreDiscreps\*.sql | New-SqlTest
#>
[CmdletBinding()]
param (
    [Parameter(Mandatory=$true, Position=0, ValueFromPipeline=$true)]
    [System.IO.FileInfo] $Path,
    [string[]] $Prefix = "",
    [string] $Suffix = ""
)
begin {
    # Try to get the user's name and email for the header.
    try {
        $UserProperties = ([adsisearcher]"samaccountname=$env:USERNAME").FindOne().Properties
        $Owners = "`r`n** Owners: $($UserProperties.displayname) <$($UserProperties.mail)>"
    }
    catch {
        $Owners = ""
    }
    # Get the path to sqlunit.pl
    $SqlUnitPl = (Split-Path -Path $MyInvocation.MyCommand.Definition -Parent) + [IO.Path]::DirectorySeparatorChar +
        'sqlunit.pl'
    # Define the SQL test query
    $SqlQuery = ""
}
process {
    # Find the deepest path that's common to all the files (https://rosettacode.org/wiki/Find_common_directory_path)
    # Get the current file's path list
    $PathList =  $Path.FullName -split "\$([IO.Path]::DirectorySeparatorChar)"
    # Get the most common path list
    if ($CommonPathList) {
        $CommonPathList = (Compare-Object -ReferenceObject $CommonPathList -DifferenceObject $PathList -IncludeEqual `
            -ExcludeDifferent -SyncWindow 0).InputObject
    } else {
        $CommonPathList = $PathList
    }
    # Get the table corresponding to the file
    $FileBaseName = ($PathList[-1] | Select-String -Pattern '[\w]+').Matches[0].Value
    $DirectoryName = $PathList[-2]
    if ($Prefix[1] -and ($FileBaseName -ne $DirectoryName)) {
        $Table = "$($Prefix[1])$FileBaseName$Suffix"
    } else {
        $Table = "$($Prefix[0])$FileBaseName$Suffix"
    }
    # Extract sqlunit statements from the file
    $SqlUnit = ((Get-Content -Raw -Path $Path) -replace "`r|`n" | Select-String -AllMatches -Pattern `
        '(?<=/\*\s*\[\s*UNIT\s*\]\s*)[^\s](\*(?!\/)|[^*])*[^\s](?=\s*\*/)').Matches.Value -join ';'
    $Sql = (swipl "$SqlUnitPl" --table "$Table" "$SqlUnit") -join "`r`n"
    if ($SqlQuery -and $Sql) { $SqlQuery += "`r`nUNION ALL`r`n" + $Sql } elseif ($Sql) { $SqlQuery = $Sql }
}
end {
    # The name of the object being tested is the basename of the deepest common path
    $ObjectName = ($CommonPathList[-1] | Select-String -Pattern '[\w]+').Matches[0].Value
    # Finally, generate the output
"/*******************************************    SQLUNIT GENERATED QUERY    ******************************************** 
** Name: ts_$ObjectName
** Description: $ObjectName unit tests generated by sqlunit (https://github.com/panasenco/sqlunit).$Owners
**********************************************************************************************************************/
$SqlQuery"
}
