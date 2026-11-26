"""Read the two name tables of src/lean_fields.f90: load() gives {Registry name: SOURCE_* class} from the select case
of field_class, gather_names() the names that gather_field handles."""
import re
from pathlib import Path

def load(path):
    text=Path(path).read_text().split('integer function field_class',1)[1].split('end function',1)[0]
    result={}
    for names,cls in re.findall(r'case\((.*?)\)\s*source=(SOURCE_\w+)',text,re.S|re.I):
        result.update((n,cls) for n in re.findall(r"'([^']+)'",names))
    return result

def gather_names(path):
    text=Path(path).read_text().split('subroutine gather_field',1)[1].split('end subroutine',1)[0]
    return {n for c in re.findall(r'case\((.*?)\)',text,re.S|re.I) for n in re.findall(r"'([^']+)'",c)}
