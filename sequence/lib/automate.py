import json

# Original data as a string
data = """
{ row : 0, 
  col : 0, 
  chip : Free, 
  card : Free_space, 
  id : 0 },
{
  row : 0,
  col : 1,
  chip : None,
  card : Reg_Card { suit : Spades, rank : Ten },
  id : 1,
}
"""

# Function to convert flattened entry to a dictionary
def convert_to_dict(entry):
    # Basic cleanup and splitting
    entry = entry.strip().strip(',').strip('{}').strip()
    items = entry.split(', ')
    
    # Parse each item into key-value pairs
    result = {}
    for item in items:
        if ' : ' in item:
            key, value = item.split(' : ', 1)
            
            # Check for nested structures like Reg_Card
            if 'Reg_Card' in value:
                value = value.replace('Reg_Card { ', '{ "type": "Reg_Card", ').replace(' }', ' }')
                value = json.loads(value.replace('suit :', '"suit":').replace('rank :', '"rank":'))
            elif value not in ['None', 'Free', 'Free_space']:  # Assume it's an integer
                value = int(value)
            else:
                value = f'"{value}"'  # Add quotes to string values

            result[key.strip()] = value

    return result

# Process the data
entries = data.split('},')
converted_data = [convert_to_dict(entry + '}') for entry in entries if entry.strip()]

# Convert to JSON
json_data = json.dumps(converted_data, indent=2)
print(json_data)
