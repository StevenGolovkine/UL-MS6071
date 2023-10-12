import os
import re

# Define the folder path containing Quarto files
folder_path = './contents/week-8'

# Get a list of Quarto files in the folder
quarto_files = [file for file in os.listdir(folder_path) if file.endswith('.qmd')]

# Define a regular expression pattern to match code chunks
code_chunk_pattern = r'```{.*?}\n(.*?)\n```'

# Initialize an empty string to accumulate code chunks
all_code_chunks = ''

# Iterate through each Quarto file
for quarto_file in quarto_files:
    quarto_file_path = os.path.join(folder_path, quarto_file)

    # Read the content of the Quarto file
    with open(quarto_file_path, 'r') as f:
        quarto_content = f.read()

    # Find all code chunks in the Quarto content
    code_chunks = re.findall(code_chunk_pattern, quarto_content, re.DOTALL)

    # Remove lines starting with '#|' from each code chunk
    cleaned_code_chunks = []
    for chunk in code_chunks:
        lines = chunk.split('\n')
        cleaned_lines = [line for line in lines if not line.startswith('#|')]
        cleaned_code_chunks.append('\n'.join(cleaned_lines))


    # Append the extracted code chunks to the accumulated string
    all_code_chunks += '\n\n'.join(cleaned_code_chunks) + '\n\n\n'

# Write the accumulated code chunks to a single R file
output_file_path = os.path.join(folder_path, 'source/all_code_chunks.R')
with open(output_file_path, 'w') as output_file:
    output_file.write(all_code_chunks)

print(f"All code chunks extracted and saved to {output_file_path}")
