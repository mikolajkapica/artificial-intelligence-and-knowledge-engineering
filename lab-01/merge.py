import os

def merge_text_files_recursive(directory_path, output_filename="merged_output.txt"):
    """
    Recursively merges the content of all text files (.txt) found within a given
    directory and its subdirectories into a single file.

    Args:
        directory_path (str): The path to the directory to start the search.
        output_filename (str, optional): The name of the file to save the merged content.
                                         Defaults to "merged_output.txt".

    Returns:
        bool: True if the merging was successful, False otherwise.
    """
    try:
        merged_content = ""
        if not os.path.isdir(directory_path):
            print(f"Error: Directory not found at '{directory_path}'")
            return False

        for root, _, files in os.walk(directory_path):
            for filename in files:
                if filename.endswith(".scala"):  # You can change the extension if needed
                    file_path = os.path.join(root, filename)
                    try:
                        with open(file_path, 'r', encoding='utf-8') as file:
                            content = file.read()
                            merged_content += content + "\n"  # Add a newline between files
                        print(f"Successfully read and merged: {file_path}")
                    except Exception as e:
                        print(f"Error reading file '{file_path}': {e}")
                        return False

        if merged_content:
            output_file_path = os.path.join(directory_path, output_filename)
            with open(output_file_path, 'w', encoding='utf-8') as outfile:
                outfile.write(merged_content)
            print(f"\nSuccessfully merged content into '{output_filename}' in '{directory_path}' and its subdirectories.")
            return True
        else:
            print(f"No text files (.scala) found in '{directory_path}' or its subdirectories.")
            return True

    except Exception as e:
        print(f"An unexpected error occurred: {e}")
        return False

if __name__ == "__main__":
    target_directory = r"""C:\Users\mkapica\IdeaProjects\artificial-intelligence-and-knowledge-engineering\lab-01\src\main\scala"""
    output_file = r"""C:\Users\mkapica\IdeaProjects\artificial-intelligence-and-knowledge-engineering\lab-01\merged-all.txt"""

    if output_file:
        success = merge_text_files_recursive(target_directory, output_file)
    else:
        success = merge_text_files_recursive(target_directory)

    if success:
        print("Text files merged successfully (recursively).")
    else:
        print("Failed to merge text files (recursively).")
